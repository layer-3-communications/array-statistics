{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NamedFieldPuns #-}

module Statistics.Array.Types
  ( AscList(..)
  , fromArray
  , unsafeFromAscendingArray
  , fromList
  , unsafeFromAscendingList
  , Quartiles(..)
  ) where

import Data.Int
import Data.Primitive.Contiguous (Contiguous, Element)
import Data.Primitive (Prim,PrimArray)

import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Primitive.Sort as Arr


-- | A non-empty list with data in ascending order.
--
-- I am keeping the implementation under-the-hood, as there are certainly better
-- ways to represent this data than an actual linked list.
newtype AscList e = AscList (PrimArray e)

fromArray :: (Prim e, Ord e)
  => PrimArray e -> Maybe (AscList e)
{-# inlineable fromArray #-}
{-# specialize fromArray :: PrimArray Int -> Maybe (AscList Int) #-}
{-# specialize fromArray :: PrimArray Int64 -> Maybe (AscList Int64) #-}
{-# specialize fromArray :: PrimArray Double -> Maybe (AscList Double) #-}
fromArray xs
  | Arr.null xs = Nothing
  | otherwise = Just $! AscList (Arr.sort xs)

-- | Unsafe because if the input array is empty or not sorted ascending, functions
-- operating on the resulting 'AscList' may (will) produce incorrect results.
unsafeFromAscendingArray :: Prim e
  => PrimArray e -> AscList e
{-# inline unsafeFromAscendingArray #-}
unsafeFromAscendingArray = AscList

fromList :: forall e. (Prim e, Ord e) => [e] -> Maybe (AscList e)
fromList xs
  | null xs = Nothing
  | otherwise = Just . AscList . Arr.sort . Arr.fromList $ xs -- WARNING inefficent, but hey... you're using lists, scrub

-- | Unsafe because if the input list is empty or not sorted ascending, functions
-- operating on the resulting 'AscList' may (will) produce incorrect results.
unsafeFromAscendingList :: forall e. Prim e => [e] -> AscList e
{-# inline unsafeFromAscendingList #-}
unsafeFromAscendingList = AscList . Arr.fromList

data Quartiles a = Quartiles
  { q1 :: !a
  , q2 :: !a
  , q3 :: !a
  }
  deriving (Show, Eq)
