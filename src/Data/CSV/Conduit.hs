{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.CSV.Conduit
    ( 
    
    -- * Fundamental Types
      CSV (..)
    , CSVSettings (..)
    , defCSVSettings
    , MapRow
    , Row
    -- * Convenience Functions
    , readCSVFile
    , transformCSV
    , mapCSVFile
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative        hiding (many)
import           Control.Exception          (bracket, SomeException)
import           Control.Monad              (mzero, mplus, foldM, when, liftM)
import           Control.Monad.IO.Class     (liftIO, MonadIO)
import           Control.Monad.Trans.Control
import           Data.Attoparsec            as P hiding (take)
import qualified Data.Attoparsec.Char8      as C8
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as B8
import           Data.ByteString.Internal   (c2w)
import           Data.Conduit as C
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary (sourceFile, sinkFile)
import qualified Data.Conduit.List as C
import           Data.Conduit.Text
import qualified Data.Map                   as M
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word                  (Word8)
import           Safe                       (headMay)
import           System.Directory
import           System.PosixCompat.Files   (getFileStatus, fileSize)
-------------------------------------------------------------------------------
import qualified Data.CSV.Conduit.Parser.ByteString as BSP
import qualified Data.CSV.Conduit.Parser.Text as TP
import           Data.CSV.Conduit.Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Represents types 'r' that are CSV-like and can be converted
-- to/from an underlying stream of type 's'.
--
-- 
-- Example #1: Basics Using Convenience API
-- 
-- @
-- import Data.Conduit
-- import Data.Conduit.Binary
-- import Data.Conduit.List as CL
-- import Data.CSV.Conduit
--
-- myProcessor :: Conduit (Row Text) m (Row Text)
-- myProcessor = CL.map reverse
-- 
-- test = runResourceT $ 
--   transformCSV defCSVSettings 
--                (sourceFile "input.csv") 
--                myProcessor
--                (sinkFile "output.csv")
-- @
--
--
-- Example #2: Basics Using Conduit API
-- 
-- @
-- import Data.Conduit
-- import Data.Conduit.Binary
-- import Data.CSV.Conduit
--
-- myProcessor :: Conduit (Row Text) m (Row Text)
-- myProcessor = undefined
--
-- test = runResourceT $ 
--   sourceFile "test/BigFile.csv" $= 
--   intoCSV defCSVSettings $=
--   myProcessor $=
--   fromCSV defCSVSettings $$
--   sinkFile "test/BigFileOut.csv"
-- @
class CSV s r where

  -----------------------------------------------------------------------------
  -- | Convert a CSV row into strict ByteString equivalent.
  rowToStr :: CSVSettings -> r -> s

  -----------------------------------------------------------------------------
  -- | Turn a stream of 's' into a stream of CSV row type. An example
  -- would be parsing a ByteString stream as rows of 'MapRow' 'Text'.
  intoCSV :: MonadResource m => CSVSettings -> Conduit s m r

  -----------------------------------------------------------------------------
  -- | Turn a stream of CSV row type back into a stream of 's'. An
  -- example would be rendering a stream of 'Row' 'ByteString' rows as
  -- 'Text'.
  fromCSV :: MonadResource m => CSVSettings -> Conduit r m s


------------------------------------------------------------------------------
-- | 'Row' instance using 'ByteString'
instance CSV ByteString (Row ByteString) where
  rowToStr s !r = 
    let 
      sep = B.pack [c2w (csvOutputColSep s)] 
      wrapField !f = case (csvOutputQuoteChar s) of
        Just !x -> x `B8.cons` escape x f `B8.snoc` x
        otherwise -> f
      escape c str = B8.intercalate (B8.pack [c,c]) $ B8.split c str
    in B.intercalate sep . map wrapField $ r

  intoCSV set = intoCSVRow (BSP.row set)
  fromCSV set = fromCSVRow set


------------------------------------------------------------------------------
-- | 'Row' instance using 'Text'
instance CSV Text (Row Text) where
  rowToStr s !r = 
    let 
      sep = T.pack [(csvOutputColSep s)] 
      wrapField !f = case (csvOutputQuoteChar s) of
        Just !x -> x `T.cons` escape x f `T.snoc` x
        otherwise -> f
      escape c str = T.intercalate (T.pack [c,c]) $ T.split (== c) str
    in T.intercalate sep . map wrapField $ r

  intoCSV set = intoCSVRow (TP.row set)
  fromCSV set = fromCSVRow set


-------------------------------------------------------------------------------
-- | 'Row' instance using 'Text' based on 'ByteString' stream
instance CSV ByteString (Row Text) where
    rowToStr s r = T.encodeUtf8 $ rowToStr s r
    intoCSV set = intoCSV set =$= C.map (map T.decodeUtf8)
    fromCSV set = fromCSV set =$= C.map T.encodeUtf8



-------------------------------------------------------------------------------
fromCSVRow set = conduitState init push close
  where
    init = ()
    push st r = return $ StateProducing st [rowToStr set r, "\n"]
    close _ = return []


-------------------------------------------------------------------------------
intoCSVRow p = parser =$= puller
  where
    parser = sequenceSink () seqSink
    seqSink _ = do
      p <- sinkParser p
      return $ Emit () [p]
    puller = do
      inc <- await
      case inc of
        Nothing -> return ()
        Just i ->
          case i of
            Just i' -> yield i' >> puller
            Nothing -> puller



-------------------------------------------------------------------------------
-- | Generic 'MapRow' instance; any stream type with a 'Row' instance
-- automatically gets a 'MapRow' instance.
instance (CSV s (Row s'), Ord s', IsString s) => CSV s (MapRow s') where
  rowToStr s r = rowToStr s . M.elems $ r
  intoCSV set = intoCSVMap set
  fromCSV set = fromCSVMap set


-------------------------------------------------------------------------------
intoCSVMap set = intoCSV set =$= converter
  where
    converter = conduitState Nothing push close 
      where
        push Nothing row = 
          case row of
            [] -> return $ StateProducing Nothing []
            xs -> return $ StateProducing (Just xs) []
        push st@(Just hs) row = return $ StateProducing st [toMapCSV hs row]
        toMapCSV !headers !fs = M.fromList $ zip headers fs
        close _ = return []


-------------------------------------------------------------------------------
fromCSVMap set = conduitState False push close
  where
    push False r = return $ StateProducing True 
                   [rowToStr set (M.keys r), "\n", rowToStr set (M.elems r), "\n"]
    push True r = return $ StateProducing True
                   [rowToStr set (M.elems r), "\n"]
    close _ = return []



                          ---------------------------
                          -- Convenience Functions --
                          ---------------------------


-------------------------------------------------------------------------------
-- | Read the entire contents of a CSV file into memory.
--
-- An easy way to run this function would be 'runResourceT' after
-- feeding it all the arguments.
readCSVFile 
    :: (MonadResource m, CSV ByteString a) 
    => CSVSettings 
    -> FilePath 
    -- ^ Input file
    -> m [a]
readCSVFile set fp = sourceFile fp $= intoCSV set $$ C.consume


-------------------------------------------------------------------------------
-- | Map over the rows of a CSV file. Provided for convenience for
-- historical reasons.
--
-- An easy way to run this function would be 'runResourceT' after
-- feeding it all the arguments.
mapCSVFile 
    :: (MonadResource m, CSV ByteString a, CSV ByteString b) 
    => CSVSettings 
    -- ^ Settings to use both for input and output
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
    :: (MonadResource m, CSV s a, CSV s' b) 
    => CSVSettings 
    -- ^ Settings to be used for input and output
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
    

                               -----------------
                               -- Simple Test --
                               -----------------


test :: IO ()
test = runResourceT $ 
  sourceFile "test/BigFile.csv" $= 
  decode utf8 $=
  (intoCSV defCSVSettings 
    :: forall m. MonadResource m => Conduit Text m (MapRow Text)) $= 
  fromCSV defCSVSettings $=
  encode utf8 $$
  sinkFile "test/BigFileOut.csv"
