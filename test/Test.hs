{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8          as B
import           Data.Map                       ((!))
import           Data.Text
import           System.Directory
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     ((@=?))

import           Data.CSV.Conduit


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [testGroup "Basic Ops" baseTests]


baseTests :: [Test]
baseTests =
  [ testCase "mapping with id works" test_identityMap
  , testCase "simple parsing works" test_simpleParse
  ]


test_identityMap :: IO ()
test_identityMap = do
    _ <- runResourceT $ mapCSVFile csvSettings f testFile2 outFile
    f1 <- readFile testFile2
    f2 <- readFile outFile
    f1 @=? f2
    removeFile outFile
  where
    outFile = "test/testOut.csv"
    f :: Row Text -> [Row Text]
    f = return


test_simpleParse :: IO ()
test_simpleParse = do
  (d :: [MapRow B.ByteString]) <- readCSVFile csvSettings testFile1
  mapM_ assertRow d
  where
    assertRow r = v3 @=? (v1 + v2)
      where v1 = readBS $ r ! "Col2"
            v2 = readBS $ r ! "Col3"
            v3 = readBS $ r ! "Sum"


csvSettings :: CSVSettings
csvSettings = defCSVSettings { csvQuoteChar = Just '`'}

testFile1, testFile2 :: FilePath
testFile1 = "test/test.csv"
testFile2 = "test/test.csv"


readBS :: B.ByteString -> Int
readBS = read . B.unpack
