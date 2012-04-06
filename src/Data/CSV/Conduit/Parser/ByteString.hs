{-| 

  This module exports the underlying Attoparsec row parser. This is helpful if
  you want to do some ad-hoc CSV string parsing.

-}

module Data.CSV.Conduit.Parser.ByteString
    ( parseCSV
    , parseRow
    , row
    , csv
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad (mzero, mplus, foldM, when, liftM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Attoparsec as P hiding (take)
import qualified Data.Attoparsec.Char8 as C8
import qualified Data.ByteString as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Internal (c2w)
import qualified Data.Map as M
import           Data.Word (Word8)
-------------------------------------------------------------------------------
import           Data.CSV.Conduit.Types


------------------------------------------------------------------------------
-- | Try to parse given string as CSV
parseCSV :: CSVSettings -> ByteString -> Either String [Row ByteString]
parseCSV s = parseOnly $ csv s


------------------------------------------------------------------------------
-- | Try to parse given string as 'Row ByteString'
parseRow :: CSVSettings -> ByteString -> Either String (Maybe (Row ByteString))
parseRow s = parseOnly $ row s


------------------------------------------------------------------------------
-- | Parse CSV
csv :: CSVSettings -> Parser [Row ByteString]
csv s = do
  r <- row s
  end <- atEnd
  case end of
    True -> case r of
      Just x -> return [x]
      Nothing -> return []
    False -> do
      rest <- csv s
      return $ case r of
        Just x -> x : rest
        Nothing -> rest


------------------------------------------------------------------------------
-- | Parse a CSV row
row :: CSVSettings -> Parser (Maybe (Row ByteString))
row csvs = csvrow csvs <|> badrow


badrow :: Parser (Maybe (Row ByteString))
badrow = P.takeWhile (not . C8.isEndOfLine) *> 
         (C8.endOfLine <|> C8.endOfInput) *> return Nothing

csvrow :: CSVSettings -> Parser (Maybe (Row ByteString))
csvrow c = 
  let rowbody = (quotedField' <|> (field c)) `sepBy` C8.char (csvSep c)
      properrow = rowbody <* (C8.endOfLine <|> P.endOfInput)
      quotedField' = case csvQuoteChar c of
          Nothing -> mzero
          Just q' -> try (quotedField q')
  in do
    res <- properrow
    return $ Just res

field :: CSVSettings -> Parser ByteString
field s = P.takeWhile (isFieldChar s) 

isFieldChar s = notInClass xs'
  where xs = csvSep s : "\n\r"
        xs' = case csvQuoteChar s of 
          Nothing -> xs
          Just x -> x : xs

quotedField :: Char -> Parser ByteString
quotedField c = 
  let quoted = string dbl *> return c
      dbl = B8.pack [c,c]
  in do
  C8.char c 
  f <- many (C8.notChar c <|> quoted)
  C8.char c 
  return $ B8.pack f


