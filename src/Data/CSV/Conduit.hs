{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.CSV.Conduit
    ( CSVeable (..)
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
import qualified Data.Map                   as M
import           Data.Word                  (Word8)
import           Safe                       (headMay)
import           System.Directory
import           System.IO
import           System.PosixCompat.Files   (getFileStatus, fileSize)
-------------------------------------------------------------------------------
import qualified Data.CSV.Conduit.Parser.ByteString as BSP
import           Data.CSV.Conduit.Types
-------------------------------------------------------------------------------


class CSVeable r where

  -----------------------------------------------------------------------------
  -- | Convert a CSV row into strict ByteString equivalent.
  rowToStr :: CSVSettings -> r -> B.ByteString

  -----------------------------------------------------------------------------
  -- | Possibly return headers.
  fileHeaders :: r -> Maybe (Row ByteString)
  fileHeaders = const Nothing
  
  -----------------------------------------------------------------------------
  intoCSV :: MonadResource m => CSVSettings -> Conduit B.ByteString m r

  -----------------------------------------------------------------------------
  fromCSV :: MonadResource m => CSVSettings -> Conduit r m B.ByteString


------------------------------------------------------------------------------
-- | 'Row' instance
instance CSVeable (Row ByteString) where
  rowToStr s !r = 
    let 
      sep = B.pack [c2w (csvOutputColSep s)] 
      wrapField !f = case (csvOutputQuoteChar s) of
        Just !x -> x `B8.cons` escape x f `B8.snoc` x
        otherwise -> f
      escape c str = B8.intercalate (B8.pack [c,c]) $ B8.split c str
    in B.intercalate sep . map wrapField $ r

  intoCSV set = parser =$= puller
    where
      parser = sequenceSink () seqSink
      seqSink _ = do
        p <- sinkParser (BSP.row set)
        return $ Emit () [p]
      puller = do
        inc <- await
        case inc of
          Nothing -> return ()
          Just i ->
            case i of
              Just i' -> yield i' >> puller
              Nothing -> puller

  fromCSV set = conduitState init push close
    where
      init = ()
      push st r = return $ StateProducing st [B.concat [rowToStr set r, "\n"]]
      close _ = return []
        
      

-------------------------------------------------------------------------------
-- | 'MapRow' Instance
instance CSVeable (MapRow ByteString) where
  rowToStr s r = rowToStr s . M.elems $ r

  fileHeaders r = Just $ M.keys r
  
  intoCSV set = intoCSV set =$= converter
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
  fromCSV set = conduitState False push close
    where
      push False r = return $ StateProducing True 
                     [B.concat [rowToStr set (M.keys r), "\n", rowToStr set (M.elems r), "\n"]]
      push True r = return $ StateProducing True
                     [B.concat [rowToStr set (M.elems r), "\n"]]
      close _ = return []
            



                               -----------------
                               -- Simple Test --
                               -----------------


test :: IO ()
test = runResourceT $ 
  sourceFile "test/BigFile.csv" $= 
  (intoCSV defCSVSettings 
    :: forall m. MonadResource m => Conduit ByteString m (MapRow ByteString)) $= 
  fromCSV defCSVSettings $$ 
  sinkFile "test/BigFileOut.csv"
