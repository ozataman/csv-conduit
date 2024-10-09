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
import           Control.Monad                    (mzero)
import           Data.Attoparsec.ByteString       as P hiding (take)
import qualified Data.Attoparsec.ByteString.Char8 as C8
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as B8
import           Data.Word                        (Word8)
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
  if end
    then case r of
      Just x -> return [x]
      Nothing -> return []
    else do
      rest <- csv s
      return $ case r of
        Just x -> x : rest
        Nothing -> rest


------------------------------------------------------------------------------
-- | Parse a CSV row
row :: CSVSettings -> Parser (Maybe (Row ByteString))
row csvs = csvrow csvs <|> badrow

csvEndOfLine :: Parser ()
csvEndOfLine = (word8 10 >> return ()) <|> (C8.string (B8.pack "\r\n") >> return ()) <|> (word8 13 >> return ())

badrow :: Parser (Maybe (Row ByteString))
badrow = P.takeWhile (not . C8.isEndOfLine) *>
         (csvEndOfLine <|> C8.endOfInput) *> return Nothing

csvrow :: CSVSettings -> Parser (Maybe (Row ByteString))
csvrow c =
  let rowbody = (quotedField' <|> field c) `sepBy` C8.char (csvSep c)
      properrow = rowbody <* (csvEndOfLine <|> P.endOfInput)
      quotedField' = case csvQuoteChar c of
          Nothing -> mzero
          Just q' -> try (quotedField q')
  in do
    res <- properrow
    return $ Just res

field :: CSVSettings -> Parser ByteString
field s = P.takeWhile (isFieldChar s)

isFieldChar :: CSVSettings -> Word8 -> Bool
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
  _ <- C8.char c
  f <- many (C8.notChar c <|> quoted)
  _ <- C8.char c
  return $ B8.pack f


