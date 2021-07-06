{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Conduit as C
import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.CSV.Conduit
import Data.CSV.Conduit.Conversion
import qualified Data.Map as Map
import qualified Data.Map.Ordered as OMap
import Data.Monoid as M
import Data.Text
import qualified Data.Vector as V
import System.Directory
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure, (@=?), (@?=))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Basic Ops" baseTests,
    testGroup "decodeCSV" decodeCSVTests
  ]

baseTests :: [Test]
baseTests =
  [ testCase "mapping with id works" test_identityMap,
    testCase "simple parsing works" test_simpleParse,
    testCase "OrderedMap" test_orderedMap
  ]

decodeCSVTests :: [Test]
decodeCSVTests =
  [ testCase "parses a CSV" $ do
      let efoos = decodeCSV defCSVSettings ("Foo\nfoo" :: B.ByteString)
      case efoos :: Either SomeException (V.Vector (Named Foo)) of
        Left e -> assertFailure (show e)
        Right foos -> V.fromList [Named Foo] @=? foos,
    testCase "eats parse errors, evidently" $ do
      let efoos = decodeCSV defCSVSettings ("Foo\nbad" :: B.ByteString)
      case efoos :: Either SomeException (V.Vector (Named Foo)) of
        Left e -> assertFailure (show e)
        Right foos -> M.mempty @=? foos
  ]

data Foo = Foo deriving (Show, Eq)

instance FromNamedRecord Foo where
  parseNamedRecord nr = do
    s <- nr .: "Foo"
    case s of
      "foo" -> pure Foo
      _ -> fail ("Expected \"foo\" but got " <> B.unpack s)

instance ToNamedRecord Foo where
  toNamedRecord Foo = namedRecord ["Foo" .= ("foo" :: B.ByteString)]

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
      where
        v1 = readBS $ r Map.! "Col2"
        v2 = readBS $ r Map.! "Col3"
        v3 = readBS $ r Map.! "Sum"

test_orderedMap :: IO ()
test_orderedMap = do
  unorderedRes <-
    C.runConduit $
      C.yieldMany [unorderedRow]
        C..| writeHeaders defCSVSettings
        C..| C.foldC
  unorderedRes @?= ("\"a\",\"b\"\n\"aval\",\"bval\"\n" :: B.ByteString)
  orderedRes <-
    C.runConduit $
      C.yieldMany [orderedRow]
        C..| writeHeadersOrdered defCSVSettings
        C..| C.foldC
  orderedRes @?= ("\"b\",\"a\"\n\"bval\",\"aval\"\n" :: B.ByteString)
  where
    orderedRow :: OrderedMapRow Text
    orderedRow = OMap.fromList pairs
    unorderedRow :: MapRow Text
    unorderedRow = Map.fromList pairs
    pairs = [("b", "bval"), ("a", "aval")]

csvSettings :: CSVSettings
csvSettings = defCSVSettings {csvQuoteChar = Just '`'}

testFile1, testFile2 :: FilePath
testFile1 = "test/test.csv"
testFile2 = "test/test.csv"

readBS :: B.ByteString -> Int
readBS = read . B.unpack
