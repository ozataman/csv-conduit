{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8          as B
import           Data.Conduit                   (($$), (=$=))
import qualified Data.Conduit.List              as CL
import           Data.Map                       ((!))
import           Data.Monoid
import           Data.Text
import qualified Data.Vector                    as V
import           System.Directory
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     ((@=?), (@?=))

import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [
    testGroup "Basic Ops" baseTests
  , testGroup "NamedE error handling" namedETests
  ]


baseTests :: [Test]
baseTests =
  [ testCase "mapping with id works" test_identityMap
  , testCase "simple parsing works" test_simpleParse
  ]


namedETests :: [Test]
namedETests =
  [ testCase "parses correctly formatted file" $ do
      res <- CL.sourceList ["foo\nbar" :: B.ByteString] =$=
        intoCSV defCSVSettings $$
        CL.consume
      res @?= [NamedE (Right (Foo Bar))]
  , testCase "retains error messages on incorrectly formatted rows and recovers" $ do
      res <- CL.sourceList ["foo\nbad\nbar" :: B.ByteString] =$=
        intoCSV defCSVSettings $$
        CL.consume
      res @?= [
          NamedE (Left "Expected token \"bar\" but got \"bad\"")
        , NamedE (Right (Foo Bar))
        ]
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
  (d :: V.Vector (MapRow B.ByteString)) <- readCSVFile csvSettings testFile1
  V.mapM_ assertRow d
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


newtype Foo = Foo { foo :: Bar } deriving (Show, Eq)


instance FromNamedRecord Foo where
  parseNamedRecord nr = fmap Foo (nr .: "foo")

data Bar = Bar deriving (Show, Eq)


instance FromField Bar where
  parseField "bar" = return Bar
  parseField f     = fail ("Expected token \"bar\" but got \"" <> B.unpack f <> "\"")
