{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.CSV.Enumerator 
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
  -- | These are some simple file-related operations for basic use cases.
  , readCSVFile
  , writeCSVFile
  , appendCSVFile

  -- * Very Basic CSV Operations (for Debugging or Quick&Dirty Needs)
  , parseCSV
  , parseRow
  
  -- * Generic Folds Over CSV Files
  -- | These operations enable you to do whatever you want with CSV files;
  -- including interleaved IO, etc.
  , foldCSVFile
  , CSVAction
  , funToIter
  , funToIterIO

  -- * Mapping Over CSV Files
  , mapCSVFile
  , mapCSVFileM
  , mapCSVFileM_
  , mapAccumCSVFile
  , mapIntoHandle

  -- * Primitive Iteratees
  , collectRows
  , outputRowIter
  , outputRowsIter

  -- * Other Utilities
  , outputRow
  , outputRows
  , outputColumns
  , writeHeaders
  )

where

import Control.Applicative hiding (many)
import Control.Exception (bracket, SomeException)
import Control.Monad (mzero, mplus, foldM, when, liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Internal (c2w)
import qualified Data.Map as M
import System.Directory
import System.IO
import System.PosixCompat.Files (getFileStatus, fileSize)

import Data.Attoparsec as P hiding (take)
import qualified Data.Attoparsec.Char8 as C8
import Data.Attoparsec.Enumerator
import qualified Data.Enumerator as E
import Data.Enumerator (($$), yield, continue)
import Data.Enumerator.Binary (enumFile)
import Data.Word (Word8)
import Safe (headMay)

import Data.CSV.Enumerator.Types
import Data.CSV.Enumerator.Parser


class CSVeable r where

  -- | Convert a CSV row into strict ByteString equivalent.
  rowToStr :: CSVSettings -> r -> B.ByteString

  -- | Possibly return headers for a list of rows.
  fileHeaders :: [r] -> Maybe Row

  -- | The raw iteratee to process any Enumerator stream
  iterCSV :: CSVSettings
          -> CSVAction r a
          -> a
          -> E.Iteratee B.ByteString IO a


  -- | Iteratee to push rows into a given file
  fileSink 
    :: CSVSettings
    -> FilePath
    -> (Maybe Handle, Int)
    -> ParsedRow r
    -> E.Iteratee B.ByteString IO (Maybe Handle, Int)


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
  rowToStr s !r = 
    let 
      sep = B.pack [c2w (csvOutputColSep s)] 
      wrapField !f = case (csvOutputQuoteChar s) of
        Just !x -> x `B8.cons` escape x f `B8.snoc` x
        otherwise -> f
      escape c str = B8.intercalate (B8.pack [c,c]) $ B8.split c str
    in B.intercalate sep . map wrapField $ r
  
  fileHeaders _ = Nothing


  iterCSV csvs f acc = loop acc
    where
      loop !acc' = do
        eof <- E.isEOF 
        case eof of
          True -> f acc' EOF
          False -> comboIter acc'
      procRow acc' = rowParser csvs >>= f acc' . ParsedRow
      comboIter acc' = procRow acc' >>= loop
      

  fileSink csvs fo = iter 
    where
      iter :: (Maybe Handle, Int) 
           -> ParsedRow Row 
           -> E.Iteratee B.ByteString IO (Maybe Handle, Int)

      iter acc@(oh, i) EOF = case oh of
          Just oh' -> liftIO (hClose oh') >> yield (Nothing, i) E.EOF
          Nothing -> yield acc E.EOF

      iter acc (ParsedRow Nothing) = yield acc (E.Chunks [])

      iter (Nothing, !i) r = do
        oh <- liftIO $ openFile fo WriteMode
        iter (Just oh, i) r

      iter (Just oh, !i) (ParsedRow (Just r)) = do 
        outputRowIter csvs oh r 
        yield (Just oh, i+1) (E.Chunks [])


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
        Just oh' -> liftIO (hClose oh') >> yield (Nothing, i) E.EOF
        Nothing -> yield acc E.EOF
      iter fi acc (ParsedRow Nothing) = return acc
      iter fi (Nothing, !i) (ParsedRow (Just r)) = do
        let row' = f r
        oh <- liftIO $ openFile fo AppendMode
        iter fi (Just oh, i) (ParsedRow (Just r))
      iter fi (Just oh, !i) (ParsedRow (Just r)) = do 
        outputRowsIter s oh (f r) 
        return (Just oh, i+1)



------------------------------------------------------------------------------
-- 'MapRow' instance for 'CSVeable'
instance CSVeable MapRow where
  rowToStr s r = rowToStr s . M.elems $ r

  fileHeaders rs = headMay rs >>= return . M.keys

  iterCSV csvs f !acc = loop ([], acc)
    where
      loop (headers, !acc') = do
        eof <- E.isEOF 
        case eof of
          True -> f acc' EOF
          False -> comboIter headers acc'

      comboIter !headers !acc' = do 
        a <- procRow headers acc' 
        loop (headers, a)

      -- Fill headers if not yet filled
      procRow [] !acc' = rowParser csvs >>= (\(Just hs) -> loop (hs, acc'))

      -- Process starting w/ the second row
      procRow !headers !acc' = rowParser csvs >>= 
                               toMapCSV headers >>= 
                               f acc' . ParsedRow 

      toMapCSV !headers !fs = yield (fs >>= (Just . M.fromList . zip headers)) (E.Chunks [])


  fileSink s fo = mapIter
    where
      mapIter :: (Maybe Handle, Int) 
              -> ParsedRow MapRow 
              -> E.Iteratee B.ByteString IO (Maybe Handle, Int)
      mapIter acc@(oh, !i) EOF = case oh of
        Just oh' -> liftIO (hClose oh') >> yield (Nothing, i) E.EOF
        Nothing -> yield acc E.EOF
      mapIter !acc (ParsedRow Nothing) = return acc
      mapIter (Nothing, !i) (ParsedRow (Just (!r))) = do
        oh <- liftIO $ do
          oh' <- openFile fo WriteMode
          B8.hPutStrLn oh' . rowToStr s . M.keys $ r
          return oh'
        mapIter (Just oh, i) (ParsedRow (Just r))
      mapIter (Just oh, !i) (ParsedRow (Just (!r))) = do
        outputRowIter s oh r 
        return (Just oh, i+1)


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
        Just oh' -> liftIO (hClose oh') >> yield (Nothing, i) E.EOF
        Nothing -> yield acc E.EOF
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
                False -> B8.hPutStrLn oh' . rowToStr s . M.keys . (addFileSource fi) $ x
              return oh'
            iter fi (Just oh, i) (ParsedRow (Just r))
      iter fi (Just oh, !i) (ParsedRow (Just r)) = 
        let rows = map (addFileSource fi) $ f r
        in do
          outputRowsIter s oh rows 
          return (Just oh, i+1)


------------------------------------------------------------------------------
-- | Open & fold over the CSV file.  
--
-- Processing starts on row 2 for MapRow instance to use first row as column
-- headers.
foldCSVFile 
  :: (CSVeable r)
  => FilePath -- ^ File to open as a CSV file
  -> CSVSettings -- ^ CSV settings to use on the input file
  -> CSVAction r a -- ^ Fold action
  -> a  -- ^ Initial accumulator
  -> IO (Either SomeException a) -- ^ Error or the resulting accumulator
foldCSVFile fp csvs f acc = E.run (enumFile fp $$ iterCSV csvs f acc)


------------------------------------------------------------------------------
-- | Take a CSV file, apply function to each of its rows and save the
-- resulting rows into a new file.
--
-- Each row is simply a list of fields.
mapCSVFile 
  :: (CSVeable r)
  => FilePath         -- ^ Input file
  -> CSVSettings      -- ^ CSV Settings
  -> (r -> [r])       -- ^ A function to map a row onto rows
  -> FilePath         -- ^ Output file
  -> IO (Either SomeException Int)    -- ^ Number of rows processed 
mapCSVFile fi s f fo = do
  res <- foldCSVFile fi s iter (Nothing, 0)
  return $ snd `fmap` res
  where
    iter !acc (ParsedRow (Just !r)) = foldM chain acc (f r) 
    iter !acc x = fileSink s fo acc x
    chain !acc !r = fileSink s fo acc (ParsedRow (Just r))


------------------------------------------------------------------------------
-- | Take a CSV file, apply an IO action to each of its rows and save the
-- resulting rows into a new file.
--
-- Each row is simply a list of fields.
mapCSVFileM
  :: (CSVeable r)
  => FilePath         -- ^ Input file
  -> CSVSettings      -- ^ CSV Settings
  -> (r -> IO [r])       -- ^ A function to map a row onto rows
  -> FilePath         -- ^ Output file
  -> IO (Either SomeException Int)    -- ^ Number of rows processed 
mapCSVFileM fi s f fo = do
  res <- foldCSVFile fi s iter (Nothing, 0)
  return $ snd `fmap` res
  where
    iter !acc (ParsedRow (Just !r)) = foldM chain acc =<< liftIO (f r)
    iter !acc x = fileSink s fo acc x
    chain !acc !r = fileSink s fo acc (ParsedRow (Just r))



------------------------------------------------------------------------------
-- | Take a CSV file, apply an IO action to each of its rows and discard the results.
--
mapCSVFileM_
  :: (CSVeable r)
  => FilePath         -- ^ Input file
  -> CSVSettings      -- ^ CSV Settings
  -> (r -> IO a)       -- ^ A function to process rows
  -> IO (Either SomeException Int)    -- ^ Number of rows processed 
mapCSVFileM_ fi s f = foldCSVFile fi s iter 0
  where
    iter !acc (ParsedRow (Just !r)) = liftIO (f r) >> return (acc+1)


------------------------------------------------------------------------------
-- | Map-accumulate over a CSV file. Similar to 'mapAccumL' in 'Data.List'.
mapAccumCSVFile
  :: (CSVeable r)
  => FilePath
  -> CSVSettings
  -> (acc -> r -> (acc, [r]))
  -> acc
  -> FilePath
  -> IO (Either SomeException acc)
mapAccumCSVFile fi s f acc fo = do
  res <- foldCSVFile fi s iter (acc, (Nothing, 0))
  return $ fst `fmap` res
  where
    iter (a, outa) (ParsedRow (Just !r)) = foldM chain (a', outa) rs
      where (a', rs) = f a r
    iter (a, outa) x = do
      outa' <- fileSink s fo outa x
      return $ (a, outa')
    chain (a, outa) !r = do
      outa' <- fileSink s fo outa (ParsedRow (Just r))
      return $ (a, outa')


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
  let doOutput h = writeHeaders s h rs >> outputRowsIter h
      outputRowsIter h = foldM (step h) 0  . map (rowToStr s) $ rs
      step h acc x = (B8.hPutStrLn h x) >> return (acc+1)
  in bracket
      (openFile fp WriteMode)
      (hClose)
      (doOutput)

------------------------------------------------------------------------------
appendCSVFile :: (CSVeable r) => CSVSettings   -- ^ CSV settings
             -> FilePath  -- ^ Target file path
             -> [r]   -- ^ Data to be output
             -> IO Int  -- ^ Number of rows written
appendCSVFile s fp rs = 
  let doOutput (c,h) = when c (writeHeaders s h rs >> return ()) >> outputRowsIter h
      outputRowsIter h = foldM (step h) 0  . map (rowToStr s) $ rs
      step h acc x = (B8.hPutStrLn h x) >> return (acc+1)
      chkOpen = do
        wrHeader <- do
          fe <- doesFileExist fp 
          if fe
            then do
              fs <- getFileStatus fp >>= return . fileSize
              return $ if fs > 0 then False else True
            else return True
        h <- openFile fp AppendMode
        return (wrHeader, h)
  in bracket
      (chkOpen)
      (hClose . snd)
      (doOutput)

------------------------------------------------------------------------------
-- | Output given row into given handle
outputRow :: CSVeable r => CSVSettings -> Handle -> r -> IO ()
outputRow s oh = B8.hPutStrLn oh . rowToStr s


outputRows :: CSVeable r => CSVSettings -> Handle -> [r] -> IO ()
outputRows s oh = mapM_ (outputRow s oh)


-- | Expand or contract the given 'MapRow' to contain exactly the given set of
-- columns and then write the row into the given 'Handle'.
--
-- This is helpful in filtering the columns or perhaps combining a number of
-- files that don't have the same columns. 
--
-- Missing columns will be left empty.
outputColumns :: CSVSettings -> Handle -> [ByteString] -> MapRow -> IO ()
outputColumns s h cs r = outputRow s h r'
  where
    r' = M.fromList $ map formCol cs
    formCol x = (x, maybe "" id $ M.lookup x r)



writeHeaders :: CSVeable r => CSVSettings -> Handle -> [r] -> IO Bool
writeHeaders s h rs = case fileHeaders rs of
  Just hs -> (B8.hPutStrLn h . rowToStr s) hs >> return True
  Nothing -> return False


outputRowIter :: CSVeable r => CSVSettings -> Handle -> r -> E.Iteratee B.ByteString IO ()
outputRowIter s oh = liftIO . outputRow s oh


outputRowsIter :: CSVeable r => CSVSettings -> Handle -> [r] -> E.Iteratee B.ByteString IO ()
outputRowsIter s oh rs = mapM_ (outputRowIter s oh) rs


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
data ParsedRow r = ParsedRow (Maybe r) | EOF


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
    iterf !acc EOF = liftIO (f acc EOF) >>= \(!acc') -> yield acc' E.EOF
    iterf !acc r = liftIO (f acc r) >>= \(!acc') -> yield acc' (E.Chunks [])


------------------------------------------------------------------------------
-- | Convenience converter for fold step functions that are pure.
--
-- Use this if you don't want to deal with Iteratees when writing your fold
-- functions.
funToIter :: (CSVeable r) => (a -> ParsedRow r -> a) -> CSVAction r a
funToIter f = iterf
  where
    iterf !acc EOF = yield (f acc EOF) E.EOF
    iterf !acc r = yield (f acc r) (E.Chunks [])



------------------------------------------------------------------------------
-- | Create an iteratee that can map over a CSV stream and output results to
-- a handle in an interleaved fashion. 
--
-- Example use: Let's map over a CSV file coming in through 'stdin' and push
-- results to 'stdout'.
--
-- > f r = return [r] -- a function that just returns the given row
--
-- > E.run (E.enumHandle 4096 stdin $$ mapIntoHandle defCSVSettings True stdout f)
--
-- This nicely allows us to do things like (assuming you have pv installed):
--
-- > pv inputFile.csv | myApp > output.CSV
--
-- And monitor the ongoing progress of processing.
mapIntoHandle 
  :: (CSVeable r)
  => CSVSettings                  -- ^ 'CSVSettings'
  -> Bool                         -- ^ Whether to write headers
  -> Handle                       -- ^ Handle to stream results
  -> (r -> IO [r])                -- ^ Map function
  -> E.Iteratee ByteString IO Int -- ^ Resulting Iteratee
mapIntoHandle csvs outh h f = do
  snd `liftM` iterCSV csvs (funToIterIO f') (False,0)
  where
    f' acc EOF = return acc
    f' acc (ParsedRow Nothing) = return acc
    f' (False, i) r'@(ParsedRow (Just r)) = do
      rs <- f r
      headerDone <- if outh then writeHeaders csvs h rs else return True
      if headerDone 
      	then f' (headerDone, 0) r'  -- Headers are done, now process row
        else return (False, i+1)    -- Problem in this row, move on to next
    f' (True, !i) (ParsedRow (Just r)) = do
      rs <- f r
      outputRows csvs h rs
      return (True, i+1)


------------------------------------------------------------------------------
-- | Just collect all rows into an array. This will cancel out the incremental
-- nature of this library.
collectRows :: CSVeable r => CSVAction r [r]
collectRows acc EOF = yield acc (E.Chunks [])
collectRows acc (ParsedRow (Just r)) = let a' = (r:acc) 
                                       in a' `seq` yield a' (E.Chunks [])
collectRows acc (ParsedRow Nothing) = yield acc (E.Chunks [])


------------------------------------------------------------------------------
-- Parsers

rowParser 
  :: (Monad m, MonadIO m) 
  => CSVSettings -> E.Iteratee B.ByteString m (Maybe Row)
rowParser csvs = E.catchError p handler 
  where 
    p = iterParser $ row csvs
    handler e = do
      liftIO $ putStrLn ("Error in parsing: " ++ show e)
      yield Nothing (E.Chunks [])
      

