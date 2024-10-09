{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.CSV.Conduit.Types where

-------------------------------------------------------------------------------
import           Data.Default
import qualified Data.Map         as M
import qualified Data.Map.Ordered as MO
-------------------------------------------------------------------------------

data QuoteEmpty = DoQuoteEmpty | DontQuoteEmpty deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | Settings for a CSV file. This library is intended to be flexible
-- and offer a way to process the majority of text data files out
-- there.
data CSVSettings = CSVSettings
  {
    -- | Separator character to be used in between fields
    csvSep       :: !Char

    -- | Quote character that may sometimes be present around fields.
    -- If 'Nothing' is given, the library will never expect quotation
    -- even if it is present.
  , csvQuoteCharAndStyle :: !(Maybe (Char, QuoteEmpty))
  } deriving (Show, Eq)


csvQuoteChar :: CSVSettings -> Maybe Char
csvQuoteChar = (fst <$>) . csvQuoteCharAndStyle

-------------------------------------------------------------------------------
-- | Default settings for a CSV file.
--
-- > csvSep = ','
-- > csvQuoteChar = Just '"'
--
defCSVSettings :: CSVSettings
defCSVSettings = CSVSettings
  { csvSep = ','
  , csvQuoteCharAndStyle = Just ('"', DoQuoteEmpty)
  }

defDontQuoteEmptyCSVSettings :: CSVSettings
defDontQuoteEmptyCSVSettings = CSVSettings
  { csvSep = ','
  , csvQuoteCharAndStyle = Just ('"', DontQuoteEmpty)
  }

instance Default CSVSettings where
    def = defCSVSettings

-------------------------------------------------------------------------------
-- | A 'Row' is just a list of fields
type Row a = [a]

-------------------------------------------------------------------------------
-- | A 'MapRow' is a dictionary based on 'Data.Map' where column names
-- are keys and row's individual cell values are the values of the
-- 'Map'.
type MapRow a = M.Map a a

-- | An 'OrderedMapRow' is a dictionary based on 'Data.Map.Ordered' where column
-- names are keys and row's individual cell values are the values of the 'OMap'.
-- Unlike 'MapRow', 'OrderedMapRow' preserves the insertion ordering of columns.
-- 'OrderedMapRow' is a reasonable default in most cases.
type OrderedMapRow a = MO.OMap a a
