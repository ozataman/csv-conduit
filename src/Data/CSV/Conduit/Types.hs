{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.CSV.Conduit.Types where

-------------------------------------------------------------------------------
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Map as M
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Settings for a CSV file. This library is intended to be flexible
-- and offer a way to process the majority of text data files out
-- there.
data CSVSettings = CSVS
  { 
    -- | Separator character to be used in between fields
    csvSep :: !Char          

    -- | Quote character that may sometimes be present around fields.
    -- If 'Nothing' is given, the library will never expect quotation
    -- even if it is present.
  , csvQuoteChar :: !(Maybe Char)
  
    -- | Quote character that should be used in the output.
  , csvOutputQuoteChar :: !(Maybe Char)
  
    -- | Field separator that should be used in the output.
  , csvOutputColSep :: !Char
  } deriving (Read, Show, Eq)



-------------------------------------------------------------------------------
-- | Default settings for a CSV file. 
--
-- > csvSep = ','
-- > csvQuoteChar = Just '"'
-- > csvOutputQuoteChar = Just '"'
-- > csvOutputColSep = ','
--
defCSVSettings :: CSVSettings
defCSVSettings = CSVS
  { csvSep = ','
  , csvQuoteChar = Just '"'
  , csvOutputQuoteChar = Just '"'
  , csvOutputColSep = ','
  } 


-------------------------------------------------------------------------------
-- | A 'Row' is just a list of fields
type Row a = [a]

-------------------------------------------------------------------------------
-- | A 'MapRow' is a dictionary based on 'Data.Map'
type MapRow a = M.Map a a
