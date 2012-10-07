module Main where


import qualified Data.ByteString.Char8 as B
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.CSV.Conduit
import           Data.Map              ((!))
import           System.Directory
import           System.Environment


-------------------------------------------------------------------------------
main = do
    fi : fo : _ <- getArgs
    runResourceT $ mapCSVFile defCSVSettings f fi fo
    -- runResourceT $ sourceFile fi $$ sinkFile fo


f :: Row B.ByteString -> [Row B.ByteString]
f = return
