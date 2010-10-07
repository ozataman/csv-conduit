{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.CSV.Iteratee 
  ( 
   -- * CSV Data types
    Row   -- Simply @[ByteString]@
  , Field   -- Simply @ByteString@
  , MapRow  

  -- * CSV Setttings
  , CSVSettings(..)
  , defCSVSettings

  -- * Mapping Over CSV Files
  , mapCSVFile
  , mapCSVMapFile

  -- * Folding Over CSV Files 
  , processCSVFile 
  , processCSVMapFile
  , CSVAction
  , CSVMapAction

  -- * Primitive Iteratees
  , collectRows
  
  -- * Output Functions
  , CSVeable
  , rowToStr
  )

where

import Control.Applicative hiding (many)
import Control.Exception (bracket, SomeException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import qualified Data.Map as M
import System.IO

import Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as C8
import Data.Attoparsec.Enum
import qualified Data.Enumerator as E
import Data.Enumerator (($$))
import Data.Enumerator.IO (enumFile)
import Data.Word (Word8)

-- | Settings for a CSV file
data CSVSettings = CSVS
  { 
    -- | Separator character to be used in between fields
    csvSep :: Char          

    -- | Quote character that may sometimes be present around fields
  , csvQuoteChar :: Char    
  }

-- | Default settings for a CSV file
defCSVSettings :: CSVSettings
defCSVSettings = CSVS
  { csvSep = ','
  , csvQuoteChar = '"'
  }

type Row = [Field]
type Field = B.ByteString
type MapRow = M.Map B.ByteString B.ByteString

class CSVeable c where
  rowToStr :: CSVSettings -> c -> B.ByteString

instance CSVeable Row where
  rowToStr s r = let sep = B.pack [c2w (csvSep s)] 
                     qt = c2w (csvQuoteChar s)
                     wrapField f = qt `B.cons` f `B.snoc` qt
                 in B.intercalate sep . map wrapField $ r

instance CSVeable MapRow where
  rowToStr s r = rowToStr s . M.elems $ r

mapCSVFile
  :: FilePath                         -- ^ Input file
  -> CSVSettings                      -- ^ CSV Settings
  -> (Row -> Row)                     -- ^ A fucntion to map rows onto rows
  -> FilePath                         -- ^ Output file
  -> IO (Either SomeException Int)    -- ^ Number of rows processed 
mapCSVFile fi s f fo = do
  oh <- openFile fo WriteMode
  res <- E.run (enumFile fi $$ loop (oh, 0))
  return $ snd `fmap` res
  where
    loop !acc'@(oh, i) = do
      eof <- E.isEOF 
      case eof of
        True -> E.yield acc' E.EOF
        False -> comboIter acc'
    comboIter acc'@(oh,i) = procRow oh >> loop (oh, i+1)
    procRow oh = rowParser s >>= appFun >>= outputRow oh

    appFun :: Maybe Row -> E.Iteratee B.ByteString IO (Maybe Row)
    appFun (Just x) = return . Just . f $ x
    appFun Nothing = return Nothing

    outputRow :: Handle -> Maybe Row -> E.Iteratee B.ByteString IO ()
    outputRow oh (Just r) = (liftIO $ B.hPutStrLn oh . rowToStr s $ r) >> return ()
    outputRow oh Nothing = return ()



mapCSVMapFile
  :: FilePath
  -> CSVSettings
  -> (MapRow -> MapRow)   -- ^ A function to map rows onto rows
  -> FilePath                         -- ^ Output file
  -> IO (Either SomeException Int)    -- ^ Number of rows processed 
mapCSVMapFile fi s f fo = do
  res <- processCSVMapFile fi s mapIter (Nothing, 0)
  return $ snd `fmap` res
  where
    mapIter :: (Maybe Handle, Int) -> Maybe MapRow -> E.Iteratee B.ByteString IO (Maybe Handle, Int)
    mapIter acc Nothing = E.yield acc E.EOF
    mapIter (Nothing, !i) (Just r) = do
      let row' = f r
      oh <- liftIO $ do
        oh' <- openFile fo WriteMode
        B.hPutStrLn oh' . rowToStr s . M.keys $ row'
        return oh'
      mapIter (Just oh, i) (Just r)
    mapIter (Just oh, !i) (Just row) = outputRow oh (f row) >> return (Just oh, i+1)

    outputRow :: Handle -> MapRow -> E.Iteratee B.ByteString IO ()
    outputRow oh = liftIO . B.hPutStrLn oh . rowToStr s


-- | An iteratee that processes each row of a CSV file and updates the accumulator.
--
-- You would implement one of these to use with process* functions.
type CSVAction a = a -> Maybe Row -> E.Iteratee B.ByteString IO a

-- | Same as 'CSVAction' but operates on Map rows.
--
-- You would implement one of these to use with process* functions.
type CSVMapAction a = a -> Maybe MapRow -> E.Iteratee B.ByteString IO a

-- | Open & fold over the CSV file. Processing starts on row 2 and each row is represented as a Map
-- using first row as column headers
processCSVMapFile
  :: FilePath -- ^ File to open as a CSV file
  -> CSVSettings
  -> CSVMapAction a -- ^ An iteratee that processes each row of a CSV file and updates the accumulator
  -> a  -- ^ Initial accumulator
  -> IO (Either SomeException a) -- ^ Error or the resulting accumulator
processCSVMapFile fp csvs f !acc = E.run (enumFile fp $$ loop [] acc)
  where
    loop !headers !acc' = do
      eof <- E.isEOF 
      case eof of
        True -> f acc' Nothing
        False -> comboIter headers acc'
    comboIter headers acc' = procRow headers acc' >>= loop headers
    procRow [] acc' = rowParser csvs >>= (\(Just headers) -> loop headers acc') -- Fill headers if not yet filled
    procRow headers acc' = rowParser csvs >>= toMapCSV headers >>= f acc' -- Process starting w/ the second row
    toMapCSV headers fs = E.yield (fs >>= (Just . M.fromList . zip headers)) (E.Chunks [])


-- | Same as 'processCSVMapFile' but treats each row as @[ByteString]@
-- Processing starts on row 1
processCSVFile
  :: FilePath -- ^ File to open as a CSV file
  -> CSVSettings
  -> CSVAction a -- ^ An iteratee that processes each row of a CSV file and updates the accumulator
  -> a  -- ^ Initial accumulator
  -> IO (Either SomeException a) -- ^ Error or the resulting accumulator
processCSVFile fp csvs f acc = E.run (enumFile fp $$ loop acc)
  where
    loop !acc' = do
      eof <- E.isEOF 
      case eof of
        True -> f acc' Nothing
        False -> comboIter acc'
    comboIter acc' = procRow acc' >>= loop
    procRow acc' = rowParser csvs >>= f acc'


-- | Just collect all rows into an array. This will cancel out the incremental nature of this library.
collectRows :: [a] -> Maybe a -> E.Iteratee B.ByteString IO [a]
collectRows acc Nothing = E.yield acc (E.Chunks [])
collectRows acc (Just row) = E.yield (row : acc) (E.Chunks [])

-- * Parsers

rowParser :: (Monad m) => CSVSettings -> E.Iteratee B.ByteString m (Maybe Row)
rowParser csvs = iterParser $ choice [csvrow csvs, badrow]

badrow :: Parser (Maybe Row)
badrow = P.takeWhile (notInClass "\n\r") *> C8.endOfLine *> return Nothing

csvrow :: CSVSettings -> Parser (Maybe Row)
csvrow c = 
  let !rowbody = (quotedField (csvQuoteChar c) <|> field) `sepBy` C8.char (csvSep c)
      !properrow = rowbody <* (C8.endOfLine <|> P.endOfInput)
  in do
    res <- properrow
    return $ Just res

field :: Parser Field
field = P.takeWhile isFieldChar <?> "Parsing a regular field"

isFieldChar = notInClass ",\n\r\""

quotedField :: Char -> Parser Field
quotedField c = let w = c2w c in do
  (C8.char c) <?> "Quote start"
  f <- many (notWord8 w <|> (string (B.pack $ [w,w]) *> return w))
  (C8.char c) <?> "Quote end"
  return $ B.pack f


