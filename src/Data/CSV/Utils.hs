{-# LANGUAGE OverloadedStrings #-}


module Data.CSV.Utils where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M

import Data.CSV.Data

-- | A nicer '!' that has better error reporting.
(!) :: (Ord a, Eq a, Show a, Show b) => M.Map a b -> a -> b
m ! f = case M.lookup f m of
  Just x -> x `seq` x
  Nothing -> error $ "Can't find field " ++ show f ++ " in the given Map " ++ show m ++ "."


-- | Read a field and cast it into a numeric type.
(!%) :: (Read a) => MapRow -> B8.ByteString -> a
r !% x = read . B8.unpack . (! x) $ r

