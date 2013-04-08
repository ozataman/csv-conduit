{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.CSV.Conduit.Types where

-------------------------------------------------------------------------------
import qualified Data.ByteString as S
import           Data.Default
import qualified Data.Map        as M
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
-------------------------------------------------------------------------------


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
  , csvQuoteChar :: !(Maybe Char)
  } deriving (Read, Show, Eq)



-------------------------------------------------------------------------------
-- | Default settings for a CSV file.
--
-- > csvSep = ','
-- > csvQuoteChar = Just '"'
--
defCSVSettings :: CSVSettings
defCSVSettings = CSVSettings
  { csvSep = ','
  , csvQuoteChar = Just '"'
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

-- | A shorthand for the ByteString case of 'MapRow'
type NamedRecord = MapRow S.ByteString


-- | A wrapper around custom haskell types that can directly be
-- converted/parsed from an incoming CSV stream.
--
-- We define this wrapper, unfortunately, to stop GHC from complaining
-- about overlapping instances. Just use 'getCustom' to get your
-- object out of the wrapper.
newtype Custom a = Custom { getCustom :: a } deriving (Eq,Show,Read,Ord)


-- | CSV data represented as a Haskell vector of vector of
-- bytestrings.
type Csv = Vector Record

-- | A record corresponds to a single line in a CSV file.
type Record = Vector Field

-- | The header corresponds to the first line a CSV file. Not all CSV
-- files have a header.
type Header = Vector Name

-- | A header has one or more names, describing the data in the column
-- following the name.
type Name = S.ByteString

-- | A single field within a record.
type Field = S.ByteString
