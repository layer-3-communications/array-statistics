{-# LANGUAGE NamedFieldPuns #-}

module Statistics.Array.Types
  ( AscList(..)
  , fromList
  , unsafeFromAscendingList
  , Quartiles(..)
  ) where

import Data.List (sort)

-- | A list with data in ascending order.
--
-- I am keeping the implementation under-the-hood, as there are certainly better
-- ways to represent this data than an actual linked list.
newtype AscList a = AscList [a]

fromList :: (Ord a) => [a] -> AscList a
fromList = AscList . sort

-- | Unsafe because if the input list is not sorted ascending, functions
-- operating on the resulting 'AscList' may (will) produce incorrect results.
unsafeFromAscendingList :: [a] -> AscList a
unsafeFromAscendingList = AscList

data Quartiles a = Quartiles
  { q1 :: a
  , q2 :: a
  , q3 :: a
  }
