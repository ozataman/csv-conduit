{-|

  This module exports the underlying Attoparsec row parser. This is helpful if
  you want to do some ad-hoc CSV string parsing.

-}

module Data.CSV.Conduit.Parser.Text
    ( parseCSV
    , parseRow
    , row
    , csv
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad          (mzero)
import           Data.Attoparsec.Text   as P hiding (take)
import qualified Data.Attoparsec.Text   as T
import           Data.Text              (Text)
import qualified Data.Text              as T
-------------------------------------------------------------------------------
import           Data.CSV.Conduit.Types
-------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Try to parse given string as CSV
parseCSV :: CSVSettings -> Text -> Either String [Row Text]
parseCSV s = parseOnly $ csv s


------------------------------------------------------------------------------
-- | Try to parse given string as 'Row Text'
parseRow :: CSVSettings -> Text -> Either String (Maybe (Row Text))
parseRow s = parseOnly $ row s


------------------------------------------------------------------------------
-- | Parse CSV
csv :: CSVSettings -> Parser [Row Text]
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
row :: CSVSettings -> Parser (Maybe (Row Text))
row csvs = csvrow csvs <|> badrow


badrow :: Parser (Maybe (Row Text))
badrow = P.takeWhile (not . T.isEndOfLine) *>
         (T.endOfLine <|> T.endOfInput) *> return Nothing

csvrow :: CSVSettings -> Parser (Maybe (Row Text))
csvrow c =
  let rowbody = (quotedField' <|> field c) `sepBy` T.char (csvSep c)
      properrow = rowbody <* (T.endOfLine <|> P.endOfInput)
      quotedField' = case csvQuoteChar c of
          Nothing -> mzero
          Just q' -> try (quotedField q')
  in do
    res <- properrow
    return $ Just res

field :: CSVSettings -> Parser Text
field s = P.takeWhile (isFieldChar s)

isFieldChar :: CSVSettings -> Char -> Bool
isFieldChar s = notInClass xs'
  where xs = csvSep s : "\n\r"
        xs' = case csvQuoteChar s of
          Nothing -> xs
          Just x -> x : xs

quotedField :: Char -> Parser Text
quotedField c = do
  let quoted = string dbl *> return c
      dbl = T.pack [c,c]
  _ <- T.char c
  f <- many (T.notChar c <|> quoted)
  _ <- T.char c
  return $ T.pack f


