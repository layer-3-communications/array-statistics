{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Statistics.Array
  ( median
  , minimum
  , maximum
  , quartiles
  , listDerivative
  , mad
  , bowleySkew
  ) where

import Prelude hiding (minimum,maximum)

import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Primitive (Prim,PrimArray)
import Data.Primitive.Contiguous (index, write)
import Statistics.Array.Types (Quartiles(..),AscList(..))

import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Primitive.Sort as Arr
import qualified Statistics.Array.Types as Asc

-- | @O(1)@ Median element.
median :: Prim a
  => AscList a -> a
{-# inlineable median #-}
{-# specialize median :: AscList Int -> Int #-}
{-# specialize median :: AscList Int64 -> Int64 #-}
{-# specialize median :: AscList Double -> Double #-}
median (AscList arr) = Arr.index arr (Arr.size arr `div` 2)

-- | @O(1)@ Minimum element.
minimum :: Prim a
  => AscList a -> a
{-# inlineable minimum #-}
{-# specialize minimum :: AscList Int -> Int #-}
{-# specialize minimum :: AscList Int64 -> Int64 #-}
{-# specialize minimum :: AscList Double -> Double #-}
minimum (AscList arr) = Arr.index arr 0

-- | @O(1)@ Maximum element.
maximum :: Prim a
  => AscList a -> a
{-# inlineable maximum #-}
{-# specialize maximum :: AscList Int -> Int #-}
{-# specialize maximum :: AscList Int64 -> Int64 #-}
{-# specialize maximum :: AscList Double -> Double #-}
maximum (AscList arr) = Arr.index arr (Arr.size arr - 1)

quartiles :: Prim a
  => AscList a -> Quartiles a
quartiles (AscList arr) = Quartiles
  { q1 = Arr.index arr (len `div` 4)
  , q2 = Arr.index arr (len `div` 2)
  , q3 = Arr.index arr (3 * len `div` 4)
  }
  where
  len = Arr.size arr



-- hmmm, I guess I'm going with Int64 here to match with Chronos
-- times will be stored in number of nanoseconds after unix epoch
-- which allows me times up to about the year 2262


-- so:
--   check skew of time diffs
--   check dispersion of time diffs (median absolute deviation)
--   check skew of data sizes
--   check dispersion of time diffs (median absolute deviation)
--   check size of data (at the mode)


-- | Median of absolute deviations.
-- This is a measure of dispersion that is more robust than standard deviation.
mad :: forall a.
     (Prim a, Ord a, Num a)
  => AscList a -> a
{-# inlineable mad #-}
{-# specialize mad :: AscList Int -> Int #-}
{-# specialize mad :: AscList Int64 -> Int64 #-}
{-# specialize mad :: AscList Double -> Double #-}
mad asc@(AscList arr) =
  let m = median asc
      devs = (\x -> abs (x - m)) `Arr.map` arr :: PrimArray a
      devsAsc = Arr.sort devs
   in median $ Asc.unsafeFromAscendingArray devsAsc

-- | Take the difference between adjacent elements. Resulting array has
-- length one less than the argument array.
listDerivative :: (Prim a, Num a)
  => AscList a -> PrimArray a
{-# inlineable listDerivative #-}
{-# specialize listDerivative :: AscList Int -> PrimArray Int #-}
{-# specialize listDerivative :: AscList Int64 -> PrimArray Int64 #-}
{-# specialize listDerivative :: AscList Double -> PrimArray Double #-}
listDerivative (AscList arr) = Arr.create $ do
  let len' = Arr.size arr - 1
  diffs <- Arr.new len'
  let go !dstIx !prevElement = if dstIx < len'
        then do
          let !currentElement = index arr (dstIx + 1)
          write diffs dstIx (currentElement - prevElement)
          go (dstIx + 1) currentElement
        else pure diffs
  go 0 (index arr 0)

bowleySkew :: (Integral a) => Quartiles a -> Double
{-# inline bowleySkew #-}
bowleySkew Quartiles{q1,q2,q3}
  -- the limit as q1 approaches q3 is of course dependant on the direction of approach in the 3d space {q1,q2,q3}
  -- realistically, I'm going to call this zero, since real data has no approach, and the dirac distribution has zero skew
  | q1 == q3 = 0.0
  -- if `q1 == q2`, then skew simplifies to `(q3 - q1) / (q3 - q1)`, or `1`
  | q2 == q1 = 1.0
  -- similarly, if `q2 == q3`, then skew simplifies to `-1`
  | q2 == q3 = -1.0
  -- I'm not sure if either of those situations are reliable indicators of skew, though
  | otherwise = (realToFrac $ q3 + q1 - 2*q2) / (realToFrac $ q3 - q1)
