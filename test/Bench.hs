
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Map ((!))
import Data.Text
import System.Directory
import System.Environment
import Data.CSV.Conduit


main = do
    inPath:_ <- getArgs
    runResourceT $ mapCSVFile defCSVSettings idF inPath outPath
    removeFile outPath
  where
    outPath = "test/testOut.csv"


idF :: Row Text -> [Row Text]
idF = return . id
