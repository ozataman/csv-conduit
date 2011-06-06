
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Map ((!))
import System.Directory

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit


import Data.CSV.Enumerator


main = defaultMain tests

tests = [ testGroup "Basic Ops" baseTests ]


baseTests = [
    testCase "mapping with id works" test_identityMap
  , testCase "simple parsing works" test_simpleParse
  ]


test_identityMap = do
  Right r <- mapCSVFile "test.csv" csvSettings f "testOut.csv"
  3 @=? r
  f1 <- readFile "test.csv"
  f2 <- readFile "testOut.csv"
  f1 @=? f2
  removeFile "testOut.csv"
  where 
    f :: MapRow -> [MapRow]
    f = return


test_simpleParse = do
  Right (d :: [MapRow]) <- readCSVFile csvSettings "test.csv" 
  mapM_ assertRow d
  where
    assertRow r = v3 @=? (v1 + v2)
      where v1 = readBS $ r ! "Col2"
            v2 = readBS $ r ! "Col3" 
            v3 = readBS $ r ! "Sum" 


csvSettings = 
  defCSVSettings { csvQuoteChar = Just '`'
                 , csvOutputQuoteChar = Just '`' }


readBS = read . B.unpack
