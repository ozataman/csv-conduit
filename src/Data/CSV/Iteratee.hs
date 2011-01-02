{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.CSV.Iteratee 
  ( 
   -- * CSV Data types
    Row   -- Simply @[ByteString]@
  , Field   -- Simply @ByteString@
  , MapRow  

  , CSVeable(..)

  , ParsedRow(..) 

  -- * CSV Setttings
  , CSVSettings(..)
  , defCSVSettings

  -- * Reading / Writing CSV Files
  , readCSVFile
  , writeCSVFile

  -- * Folding Over CSV Files 
  -- | These enumerators generalize the map* family of functions with a running accumulator.
  , CSVAction
  , funToIter
  , funToIterIO

  -- * Primitive Iteratees
  , collectRows
  )

where

import Control.Applicative hiding (many)
import Control.Exception (bracket, SomeException)
import Control.Monad (mzero, mplus, foldM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (c2w)
import qualified Data.Map as M
import System.Directory
import System.IO

import Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as C8
-- import Data.Attoparsec.Enum
import Data.Attoparsec.Enumerator
import qualified Data.Enumerator as E
import Data.Enumerator (($$))
import Data.Enumerator.IO (enumFile)
import Data.Word (Word8)
import Safe (headMay)

import Data.CSV.Data

class CSVeable r where

  -- | Convert a CSV row into strict ByteString equivalent.
  rowToStr :: CSVSettings -> r -> B.ByteString

  -- | Possibly return headers for a list of rows.
  fileHeaders :: [r] -> Maybe Row

  -- | Open & fold over the CSV file.  Processing starts on row 2 for MapRow
  -- instance to use first row as column headers.
  foldCSVFile :: FilePath -- ^ File to open as a CSV file
              -> CSVSettings -- ^ CSV settings to use on the input file
              -> CSVAction r a -- ^ Fold action
              -> a  -- ^ Initial accumulator
              -> IO (Either SomeException a) -- ^ Error or the resulting accumulator

  -- | Take a CSV file, apply function to each of its rows and save the
  -- resulting rows into a new file.
  --
  -- Each row is simply a list of fields.
  mapCSVFile :: FilePath         -- ^ Input file
             -> CSVSettings      -- ^ CSV Settings
             -> (r -> [r])       -- ^ A function to map a row onto rows
             -> FilePath         -- ^ Output file
             -> IO (Either SomeException Int)    -- ^ Number of rows processed 

  ----------------------------------------------------------------------------
  -- | Like 'mapCSVFile' but operates on multiple files pouring results into
  -- a single file.
  mapCSVFiles :: [FilePath]       -- ^ Input files
              -> CSVSettings      -- ^ CSV Settings
              -> (r -> [r])       -- ^ A function to map a row onto rows
              -> FilePath         -- ^ Output file
              -> IO (Either SomeException Int)    -- ^ Number of rows processed 

------------------------------------------------------------------------------
-- | 'Row' instance for 'CSVeable'
instance CSVeable Row where
  rowToStr s r = let sep = B.pack [c2w (csvOutputColSep s)] 
                     qt = c2w (csvOutputQuoteChar s)
                     wrapField f = qt `B.cons` f `B.snoc` qt
                 in B.intercalate sep . map wrapField $ r
  
  fileHeaders _ = Nothing

  foldCSVFile fp csvs f acc = E.run (enumFile fp $$ loop acc)
    where
      loop !acc' = do
        eof <- E.isEOF 
        case eof of
          True -> f acc' EOF
          False -> comboIter acc'
      comboIter acc' = procRow acc' >>= loop
      procRow acc' = rowParser csvs >>= f acc' . ParsedRow

  mapCSVFile fi s f fo = do
    res <- foldCSVFile fi s iter (Nothing, 0)
    return $ snd `fmap` res
    where
      iter :: (Maybe Handle, Int) -> ParsedRow Row -> E.Iteratee B.ByteString IO (Maybe Handle, Int)
      iter acc@(oh, i) EOF = case oh of
        Just oh' -> liftIO (hClose oh') >> E.yield (Nothing, i) E.EOF
        Nothing -> E.yield acc E.EOF
      iter acc (ParsedRow Nothing) = return acc
      iter (Nothing, !i) (ParsedRow (Just r)) = do
        let row' = f r
        oh <- liftIO $ openFile fo WriteMode
        iter (Just oh, i) (ParsedRow (Just r))
      iter (Just oh, !i) (ParsedRow (Just r)) = outputRow s oh (f r) >> return (Just oh, i+1)


  mapCSVFiles fis s f fo = foldM stepFile (Right 0) fis
    where
      stepFile :: (Either SomeException Int) 
               -> FilePath 
               -> IO (Either SomeException Int)
      stepFile res0 fi = do 
        case res0 of
          Left x -> return $ Left x
          Right i -> do 
            res <- foldCSVFile fi s (iter fi) (Nothing, i)
            return $ fmap snd res

      iter :: FilePath
           -> (Maybe Handle, Int) 
           -> ParsedRow Row 
           -> E.Iteratee B.ByteString IO (Maybe Handle, Int)
      iter fi acc@(oh, i) EOF = case oh of
        Just oh' -> liftIO (hClose oh') >> E.yield (Nothing, i) E.EOF
        Nothing -> E.yield acc E.EOF
      iter fi acc (ParsedRow Nothing) = return acc
      iter fi (Nothing, !i) (ParsedRow (Just r)) = do
        let row' = f r
        oh <- liftIO $ openFile fo AppendMode
        iter fi (Just oh, i) (ParsedRow (Just r))
      iter fi (Just oh, !i) (ParsedRow (Just r)) = outputRow s oh (f r) >> return (Just oh, i+1)



------------------------------------------------------------------------------
-- 'MapRow' instance for 'CSVeable'
instance CSVeable MapRow where
  rowToStr s r = rowToStr s . M.elems $ r

  fileHeaders rs = headMay rs >>= return . M.keys

  foldCSVFile fp csvs f !acc = E.run (enumFile fp $$ loop [] acc)
    where
      loop !headers !acc' = do
        eof <- E.isEOF 
        case eof of
          True -> f acc' EOF
          False -> comboIter headers acc'
      comboIter headers acc' = procRow headers acc' >>= loop headers

      -- Fill headers if not yet filled
      procRow [] acc' = rowParser csvs >>= (\(Just headers) -> loop headers acc') 

      -- Process starting w/ the second row
      procRow headers acc' = rowParser csvs >>= toMapCSV headers >>= f acc' . ParsedRow 
      toMapCSV headers fs = E.yield (fs >>= (Just . M.fromList . zip headers)) (E.Chunks [])

  mapCSVFile fi s f fo = do
    res <- foldCSVFile fi s mapIter (Nothing, 0)
    return $ snd `fmap` res
    where
      mapIter :: (Maybe Handle, Int) -> ParsedRow MapRow -> E.Iteratee B.ByteString IO (Maybe Handle, Int)
      mapIter acc@(oh, i) EOF = case oh of
        Just oh' -> liftIO (hClose oh') >> E.yield (Nothing, i) E.EOF
        Nothing -> E.yield acc E.EOF
      mapIter acc (ParsedRow Nothing) = return acc
      mapIter (Nothing, !i) (ParsedRow (Just r)) = do
        case f r of
          [] -> return (Nothing, i) -- the fn did not return any rows at all!
          (x:_) -> do
            oh <- liftIO $ do
              oh' <- openFile fo WriteMode
              B.hPutStrLn oh' . rowToStr s . M.keys $ x
              return oh'
            mapIter (Just oh, i) (ParsedRow (Just r))
      mapIter (Just oh, !i) (ParsedRow (Just r)) = outputRow s oh (f r) >> return (Just oh, i+1)


  mapCSVFiles fis s f fo = foldM stepFile (Right 0) fis
    where
      stepFile res0 fi = do 
        case res0 of
          Left x -> return $ Left x
          Right i -> do 
            res <- foldCSVFile fi s (iter fi) (Nothing, i)
            return $ fmap snd res

      addFileSource fi r = M.insert "FromFile" (B8.pack fi) r

      iter :: FilePath
           -> (Maybe Handle, Int) 
           -> ParsedRow MapRow 
           -> E.Iteratee B.ByteString IO (Maybe Handle, Int)
      iter fi acc@(oh, i) EOF = case oh of
        Just oh' -> liftIO (hClose oh') >> E.yield (Nothing, i) E.EOF
        Nothing -> E.yield acc E.EOF
      iter fi acc (ParsedRow Nothing) = return acc
      iter fi (Nothing, !i) (ParsedRow (Just r)) = do
        case f r of
          [] -> return (Nothing, i) -- the fn did not return any rows at all!
          (x:_) -> do
            oh <- liftIO $ do
              exist <- doesFileExist fo
              oh' <- openFile fo AppendMode
              case exist of
                True -> return ()
                False -> B.hPutStrLn oh' . rowToStr s . M.keys . (addFileSource fi) $ x
              return oh'
            iter fi (Just oh, i) (ParsedRow (Just r))
      iter fi (Just oh, !i) (ParsedRow (Just r)) = 
        let rows = f . addFileSource fi $ r
        in do
          outputRow s oh rows 
          return (Just oh, i+1)


------------------------------------------------------------------------------
readCSVFile :: (CSVeable r) => CSVSettings  -- ^ CSV settings
            -> FilePath   -- ^ FilePath
            -> IO (Either SomeException [r])  -- ^ Collected data
readCSVFile s fp = do
  res <- foldCSVFile fp s collectRows []
  return $ case res of
    Left e -> Left e
    Right rs -> Right (reverse rs)


------------------------------------------------------------------------------
writeCSVFile :: (CSVeable r) => CSVSettings   -- ^ CSV settings
             -> FilePath  -- ^ Target file path
             -> [r]   -- ^ Data to be output
             -> IO Int  -- ^ Number of rows written
writeCSVFile s fp rs = 
  let doOutput h = writeHeaders h >> outputRows h
      writeHeaders h = case fileHeaders rs of
        Just hs -> B.hPutStrLn h . rowToStr s $ hs
        Nothing -> return ()
      outputRows h = foldM (step h) 0  . map (rowToStr s) $ rs
      step h acc x = (B.hPutStrLn h x) >> return (acc+1)
  in bracket
      (openFile fp WriteMode)
      (hClose)
      (doOutput)


------------------------------------------------------------------------------
-- | Output given row into given handle
outputRow :: CSVeable r => CSVSettings -> Handle -> [r] -> E.Iteratee B.ByteString IO ()
outputRow s oh = liftIO . mapM_ (B.hPutStrLn oh) . map (rowToStr s)


------------------------------------------------------------------------------
-- | A datatype that incorporates the signaling of parsing status to the
--user-developed iteratee.
--
-- We need this because some iteratees do interleaved IO (such as outputting to
-- a file via a handle inside the accumulator) and some final actions may need
-- to be taken upon encountering EOF (such as closing the interleaved handle).
--
-- Use this datatype when developing iteratees for use with fold* family of
-- functions (Row enumarators).
data (CSVeable r) => ParsedRow r = ParsedRow (Maybe r) | EOF

------------------------------------------------------------------------------
-- | An iteratee that processes each row of a CSV file and updates the
-- accumulator.
--
-- You would implement one of these to use with the 'foldCSVFile' function.
type CSVAction r a = a -> ParsedRow r -> E.Iteratee B.ByteString IO a


------------------------------------------------------------------------------
-- | Convenience converter for fold step functions that live in the IO monad.
--
-- Use this if you don't want to deal with Iteratees when writing your fold
-- functions.
funToIterIO :: (CSVeable r) => (a -> ParsedRow r -> IO a) -> CSVAction r a
funToIterIO f = iterf
  where
    iterf !acc EOF = liftIO (f acc EOF) >>= \(!acc') -> E.yield acc' E.EOF
    iterf !acc r = liftIO (f acc r) >>= \(!acc') -> E.yield acc' (E.Chunks [])


------------------------------------------------------------------------------
-- | Convenience converter for fold step functions that are pure.
--
-- Use this if you don't want to deal with Iteratees when writing your fold
-- functions.
funToIter :: (CSVeable r) => (a -> ParsedRow r -> a) -> CSVAction r a
funToIter f = iterf
  where
    iterf !acc EOF = E.yield (f acc EOF) E.EOF
    iterf !acc r = E.yield (f acc r) (E.Chunks [])


------------------------------------------------------------------------------
-- | Just collect all rows into an array. This will cancel out the incremental
-- nature of this library.
collectRows :: CSVeable r => CSVAction r [r]
collectRows acc EOF = E.yield acc (E.Chunks [])
collectRows acc (ParsedRow (Just r)) = let a' = (r:acc) 
                                       in a' `seq` E.yield a' (E.Chunks [])
collectRows acc (ParsedRow Nothing) = E.yield acc (E.Chunks [])

-- * Parsers

rowParser :: (Monad m) => CSVSettings -> E.Iteratee B.ByteString m (Maybe Row)
rowParser csvs = iterParser $ row csvs

row :: CSVSettings -> Parser (Maybe Row)
row csvs = csvrow csvs <|> badrow

badrow :: Parser (Maybe Row)
badrow = P.takeWhile (not . C8.isEndOfLine) *> 
         (C8.endOfLine <|> C8.endOfInput) *> return Nothing

csvrow :: CSVSettings -> Parser (Maybe Row)
csvrow c = 
  let !rowbody = (quotedField' <|> (field c)) `sepBy` C8.char (csvSep c)
      !properrow = rowbody <* (C8.endOfLine <|> P.endOfInput)
      quotedField' = case csvQuoteChar c of
          Nothing -> mzero
          Just q' -> try (quotedField q')
  in do
    res <- properrow
    return $ Just res

field :: CSVSettings -> Parser Field
field s = P.takeWhile (isFieldChar s) <?> "Parsing a regular field"

isFieldChar s = notInClass xs'
  where xs = csvSep s : "\n\r"
        xs' = case csvQuoteChar s of 
          Nothing -> xs
          Just x -> x : xs

quotedField :: Char -> Parser Field
quotedField c = let w = c2w c in do
  (C8.char c) <?> "Quote start"
  f <- many (notWord8 w <|> (string (B.pack $ [w,w]) *> return w))
  (C8.char c) <?> "Quote end"
  return $ B.pack f


