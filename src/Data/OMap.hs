
module Data.OMap
  ( OMap(..)
  , Data.OMap.olookup
  ) where

------------------------------------------------------------------------------
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | An \"ordered\" map that keeps data ordered in a vector, but also gives
-- you map-like access to that vector via a map of keys to int indices into
-- the vector.
data OMap a = OMap
    { omapIndMap :: Map a Int
    , omapVector :: Vector a
    }


olookup :: Ord a => a -> OMap a -> Maybe a
olookup k (OMap m v) = (v V.!?) =<< (M.lookup k m)
