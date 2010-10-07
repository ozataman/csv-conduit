{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.ByteString.Char8 as BS

import Data.Enumerator (liftI, Stream(..), yield)
import Data.CSV.Iteratee

import Data.List.Utils (join)
import System.IO
import Control.Monad.IO.Class (liftIO)

main = do
  h <- openFile "Examples/copied.csv" WriteMode
  res1 <- processCSVFile "Examples/test.csv" defCSVSettings (countAndOutput h) 0
  putStrLn $ show res1


countRow !acc row = return $ acc + 1
outputRow acc row = return $ row : acc
countAndOutput h !acc Nothing = yield acc EOF
countAndOutput h !acc (Just row) = do
  liftIO $ BS.hPutStrLn h $ BS.concat row
  return $ acc + 1
  
