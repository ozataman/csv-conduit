{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.CSV.Conduit
    (

    -- * Key Operations
      CSV (..)
    , writeHeaders

    -- * Convenience Functions
    , readCSVFile
    , writeCSVFile
    , transformCSV
    , mapCSVFile

    -- * Important Types
    , CSVSettings (..)
    , defCSVSettings
    , MapRow
    , Row

    -- * Re-exported For Convenience
    , runResourceT
    ) where

-------------------------------------------------------------------------------
import           Data.Attoparsec.Types              (Parser)
import qualified Data.ByteString                    as B
import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as B8
import           Data.ByteString.Internal           (c2w)
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary                (sinkFile, sinkIOHandle,
                                                     sourceFile)
import qualified Data.Conduit.List                  as C
import qualified Data.Map                           as M
import           Data.String
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           System.IO
-------------------------------------------------------------------------------
import qualified Data.CSV.Conduit.Parser.ByteString as BSP
import qualified Data.CSV.Conduit.Parser.Text       as TP
import           Data.CSV.Conduit.Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Represents types 'r' that are CSV-like and can be converted
-- to/from an underlying stream of type 's'.
--
--
-- Example #1: Basics Using Convenience API
--
-- >import Data.Conduit
-- >import Data.Conduit.Binary
-- >import Data.Conduit.List as CL
-- >import Data.CSV.Conduit
-- >
-- >myProcessor :: Conduit (Row Text) m (Row Text)
-- >myProcessor = CL.map reverse
-- >
-- >test = runResourceT $
-- >  transformCSV defCSVSettings
-- >               (sourceFile "input.csv")
-- >               myProcessor
-- >               (sinkFile "output.csv")
--
--
-- Example #2: Basics Using Conduit API
--
-- >import Data.Conduit
-- >import Data.Conduit.Binary
-- >import Data.CSV.Conduit
-- >
-- >myProcessor :: Conduit (MapRow Text) m (MapRow Text)
-- >myProcessor = undefined
-- >
-- >test = runResourceT $
-- >  sourceFile "test/BigFile.csv" $=
-- >  intoCSV defCSVSettings $=
-- >  myProcessor $=
-- >  (writeHeaders defCSVSettings >> fromCSV defCSVSettings) $$
-- >  sinkFile "test/BigFileOut.csv"
class CSV s r where

  -----------------------------------------------------------------------------
  -- | Convert a CSV row into strict ByteString equivalent.
  rowToStr :: CSVSettings -> r -> s

  -----------------------------------------------------------------------------
  -- | Turn a stream of 's' into a stream of CSV row type. An example
  -- would be parsing a ByteString stream as rows of 'MapRow' 'Text'.
  intoCSV :: (MonadThrow m) => CSVSettings -> GLInfConduit s m r

  -----------------------------------------------------------------------------
  -- | Turn a stream of CSV row type back into a stream of 's'. An
  -- example would be rendering a stream of 'Row' 'ByteString' rows as
  -- 'Text'.
  fromCSV :: Monad m => CSVSettings -> GInfConduit r m s





------------------------------------------------------------------------------
-- | 'Row' instance using 'ByteString'
instance CSV ByteString (Row ByteString) where
  rowToStr s !r =
    let
      sep = B.pack [c2w (csvSep s)]
      wrapField !f = case csvQuoteChar s of
        Just !x -> (x `B8.cons` escape x f) `B8.snoc` x
        _ -> f
      escape c str = B8.intercalate (B8.pack [c,c]) $ B8.split c str
    in B.intercalate sep . map wrapField $ r

  intoCSV set = intoCSVRow (BSP.row set)
  fromCSV set = fromCSVRow set


------------------------------------------------------------------------------
-- | 'Row' instance using 'Text'
instance CSV Text (Row Text) where
  rowToStr s !r =
    let
      sep = T.pack [csvSep s]
      wrapField !f = case csvQuoteChar s of
        Just !x -> x `T.cons` escape x f `T.snoc` x
        _ -> f
      escape c str = T.intercalate (T.pack [c,c]) $ T.split (== c) str
    in T.intercalate sep . map wrapField $ r

  intoCSV set = intoCSVRow (TP.row set)
  fromCSV set = fromCSVRow set


-------------------------------------------------------------------------------
-- | 'Row' instance using 'Text' based on 'ByteString' stream
instance CSV ByteString (Row Text) where
    rowToStr s r = T.encodeUtf8 $ rowToStr s r
    intoCSV set = intoCSV set >+> C.map (map T.decodeUtf8)
    fromCSV set = fromCSV set >+> C.map T.encodeUtf8



-------------------------------------------------------------------------------
-- | 'Row' instance using 'String' based on 'ByteString' stream.
-- Please note this uses the ByteString operations underneath and has
-- lots of unnecessary overhead. Included for convenience.
instance CSV ByteString (Row String) where
    rowToStr s r = rowToStr s $ map B8.pack r
    intoCSV set = intoCSV set >+> C.map (map B8.unpack)
    fromCSV set = C.map (map B8.pack) >+> fromCSV set



-------------------------------------------------------------------------------
fromCSVRow :: (Monad m, IsString s, CSV s r)
           => CSVSettings -> GInfConduit r m s
fromCSVRow set = do
  erow <- awaitE
  case erow of
    Left ures -> return ures
    Right row -> mapM_ yield [rowToStr set row, "\n"] >> fromCSVRow set


-------------------------------------------------------------------------------
intoCSVRow :: (MonadThrow m, AttoparsecInput i)
           => Parser i (Maybe o) -> GLInfConduit i m o
intoCSVRow p = parse >+> puller
  where
    parse = {-# SCC "conduitParser_p" #-} conduitParser p
    puller = {-# SCC "puller" #-} do
      emrow <- awaitE
      case emrow of
        Left ures -> return ures
        Right (_, mrow) ->
          case mrow of
            Just row -> yield row >> puller
            Nothing -> puller



-------------------------------------------------------------------------------
-- | Generic 'MapRow' instance; any stream type with a 'Row' instance
-- automatically gets a 'MapRow' instance.
instance (CSV s (Row s'), Ord s', IsString s) => CSV s (MapRow s') where
  rowToStr s r = rowToStr s . M.elems $ r
  intoCSV set = intoCSVMap set
  fromCSV set = fromCSVMap set


-------------------------------------------------------------------------------
intoCSVMap :: (Ord a, MonadThrow m, CSV s [a])
           => CSVSettings -> GLInfConduit s m (MapRow a)
intoCSVMap set = intoCSV set >+> (headers >>= converter)
  where
    headers = do
      mrow <- await
      case mrow of
        Nothing -> return []
        Just [] -> headers
        Just hs -> return hs
    converter hs = do
      erow <- awaitE
      case erow of
        Left ures -> return ures
        Right row -> yield (toMapCSV hs row) >> converter hs
    toMapCSV !hs !fs = M.fromList $ zip hs fs


-------------------------------------------------------------------------------
fromCSVMap :: (Monad m, IsString s, CSV s [a])
           => CSVSettings -> GInfConduit (M.Map k a) m s
fromCSVMap set = do
  erow <- awaitE
  case erow of
    Left ures -> return ures
    Right row -> push row >> fromCSVMap set
  where
    push r = mapM_ yield [rowToStr set (M.elems r), "\n"]


-------------------------------------------------------------------------------
-- | Write headers AND the row into the output stream, once. Just
-- chain this using the 'Monad' instance in your pipeline:
--
-- > ... =$= writeHeaders settings >> fromCSV settings $$ sinkFile "..."
writeHeaders
    :: (Monad m, CSV s (Row r), IsString s)
    => CSVSettings
    -> GConduit (MapRow r) m s
writeHeaders set = do
  mrow <- await
  case mrow of
    Nothing -> return ()
    Just row -> mapM_ yield [ rowToStr set (M.keys row)
                            , "\n"
                            , rowToStr set (M.elems row)
                            , "\n" ]


                          ---------------------------
                          -- Convenience Functions --
                          ---------------------------


-------------------------------------------------------------------------------
-- | Read the entire contents of a CSV file into memory.
--
-- An easy way to run this function would be 'runResourceT' after
-- feeding it all the arguments.
readCSVFile
    :: (CSV ByteString a)
    => CSVSettings
    -- ^ Settings to use in deciphering stream
    -> FilePath
    -- ^ Input file
    -> IO [a]
readCSVFile set fp = runResourceT $ sourceFile fp $= intoCSV set $$ C.consume



-------------------------------------------------------------------------------
-- | Write CSV data into file.
writeCSVFile
  :: (CSV ByteString a)
  => CSVSettings
  -- ^ CSV Settings
  -> FilePath
  -- ^ Target file
  -> IOMode
  -- ^ Write vs. append mode
  -> [a]
  -- ^ List of rows
  -> IO ()
writeCSVFile set fo fmode rows = runResourceT $ do
  C.sourceList rows $= fromCSV set $$
    sinkIOHandle (openFile fo fmode)


-------------------------------------------------------------------------------
-- | Map over the rows of a CSV file. Provided for convenience for
-- historical reasons.
--
-- An easy way to run this function would be 'runResourceT' after
-- feeding it all the arguments.
mapCSVFile
    :: (MonadResource m, MonadThrow m, CSV ByteString a, CSV ByteString b)
    => CSVSettings
    -- ^ Settings to use both for both input and output
    -> (a -> [b])
    -- ^ A mapping function
    -> FilePath
    -- ^ Input file
    -> FilePath
    -- ^ Output file
    -> m ()
mapCSVFile set f fi fo =
  transformCSV set (sourceFile fi) (C.concatMap f) (sinkFile fo)


-------------------------------------------------------------------------------
-- | General purpose CSV transformer. Apply a list-like processing
-- function from 'Data.Conduit.List' to the rows of a CSV stream. You
-- need to provide a stream data source, a transformer and a stream
-- data sink.
--
-- An easy way to run this function would be 'runResourceT' after
-- feeding it all the arguments.
--
-- Example - map a function over the rows of a CSV file:
--
-- > transformCSV set (sourceFile inFile) (C.map f) (sinkFile outFile)
transformCSV
    :: (MonadThrow m, CSV s a, CSV s' b)
    => CSVSettings
    -- ^ Settings to be used for both input and output
    -> Source m s
    -- ^ A raw stream data source. Ex: 'sourceFile inFile'
    -> Conduit a m b
    -- ^ A transforming conduit
    -> Sink s' m ()
    -- ^ A raw stream data sink. Ex: 'sinkFile outFile'
    -> m ()
transformCSV set source c sink =
    source $=
    intoCSV set $=
    c $=
    fromCSV set $$
    sink

