{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.CSV.Conduit
    ( CSVeable (..)
    , CSVSettings (..)
    , defCSVSettings
    , MapRow
    , Row
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative        hiding (many)
import           Control.Exception          (bracket, SomeException)
import           Control.Monad              (mzero, mplus, foldM, when, liftM)
import           Control.Monad.IO.Class     (liftIO, MonadIO)
import           Data.Attoparsec            as P hiding (take)
import qualified Data.Attoparsec.Char8      as C8
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as B8
import           Data.ByteString.Internal   (c2w)
import           Data.Conduit as C
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary
import           Data.Conduit.Text
import qualified Data.Map                   as M
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
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
-- | Represents types 'r' that can be converted from an underlying
-- stream of type 's'.
--
-- Example processing using MapRow Text isntance:
--
-- @
-- test :: IO ()
-- test = runResourceT $ 
--   sourceFile "test/BigFile.csv" $= 
--   decode utf8 $=
--   intoCSV defCSVSettings $=
--   myMapRowProcessingConduit $=
--   fromCSV defCSVSettings $=
--   encode utf8 $$
--   sinkFile "test/BigFileOut.csv"
-- @
class CSVeable s r where

  -----------------------------------------------------------------------------
  -- | Convert a CSV row into strict ByteString equivalent.
  rowToStr :: CSVSettings -> r -> s

  -----------------------------------------------------------------------------
  -- | Possibly return headers.
  fileHeaders :: r -> Maybe (Row s)
  fileHeaders = const Nothing
  
  -----------------------------------------------------------------------------
  -- | Turn a stream of 's' into a stream of CSV row type
  intoCSV :: MonadResource m => CSVSettings -> Conduit s m r

  -----------------------------------------------------------------------------
  -- | Turn a stream of CSV row type back into a stream of 's'
  fromCSV :: MonadResource m => CSVSettings -> Conduit r m s


------------------------------------------------------------------------------
-- | 'Row' instance using 'ByteString'
instance CSVeable ByteString (Row ByteString) where
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
instance CSVeable Text (Row Text) where
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
instance (CSVeable s (Row s), Ord s, IsString s) => CSVeable s (MapRow s) where
  rowToStr s r = rowToStr s . M.elems $ r
  fileHeaders r = Just $ M.keys r
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
