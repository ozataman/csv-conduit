
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Map ((!))
import System.Directory
import Data.CSV.Enumerator


main = mapCSVFile "BigFile.csv" defCSVSettings idF "TestOut.csv"


idF :: MapRow -> [MapRow]
idF = return . id
