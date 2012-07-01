
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Map ((!))
import Data.Text
import System.Directory

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit


import Data.CSV.Conduit


main = defaultMain tests

tests = [ testGroup "Basic Ops" baseTests ]


baseTests = [
    testCase "mapping with id works" test_identityMap
  , testCase "simple parsing works" test_simpleParse
  ]


test_identityMap = do
  _ <- runResourceT $ mapCSVFile csvSettings f testFile outFile
  f1 <- readFile testFile
  f2 <- readFile outFile
  f1 @=? f2
  removeFile outFile
  where
    outFile = "test/testOut.csv"
    f :: Row Text -> [Row Text]
    f = return


test_simpleParse = do
  (d :: [MapRow B.ByteString]) <- runResourceT
                                $ readCSVFile csvSettings testFile
  mapM_ assertRow d
  where
    assertRow r = v3 @=? (v1 + v2)
      where v1 = readBS $ r ! "Col2"
            v2 = readBS $ r ! "Col3"
            v3 = readBS $ r ! "Sum"


csvSettings = 
  defCSVSettings { csvQuoteChar = Just '`'
                 , csvOutputQuoteChar = Just '`' }

testFile = "test/test.csv"

readBS = read . B.unpack
