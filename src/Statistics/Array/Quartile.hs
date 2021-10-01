{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Statistics.Array.Quartile
  ( median
  , quartiles
  , listDerivative
  , mad
  , bowleySkew
  ) where

import Control.Monad (forM_)
import Data.Primitive.Contiguous (Contiguous, Element, index, write)
import Statistics.Array.Types (Quartiles(..),AscList(..))

import qualified Data.Primitive.Contiguous as Arr
import qualified Statistics.Array.Types as Asc

median :: (Contiguous arr, Element arr a)
  => AscList arr a -> a
median (AscList arr) = Arr.index arr (Arr.size arr `div` 2)

quartiles :: (Contiguous arr, Element arr a)
  => AscList arr a -> Quartiles a
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
-- THis is a measure of dispersion that is more robust than standard deviation.
mad :: forall arr a.
     (Contiguous arr, Element arr a, Ord a, Num a)
  => AscList arr a -> a
mad asc@(AscList arr) =
  let m = median asc
      devs = (\x -> abs (x - m)) `Arr.map` arr :: arr a
   in median $ Asc.fromArray devs

listDerivative :: (Contiguous arr, Element arr a, Num a)
  => AscList arr a -> arr a
listDerivative (AscList arr)
  | Arr.null arr = Arr.empty
  | otherwise = Arr.create $ do
    let len' = Arr.size arr - 1
    diffs <- Arr.new len'
    forM_ [0 .. len' - 1] $ \i -> do
      write diffs i (index arr (i + 1) - index arr i)
    pure diffs

bowleySkew :: (Integral a) => Quartiles a -> a
bowleySkew Quartiles{q1,q2,q3}
  -- the limit as q1 approaches q3 is of course dependant on the direction of approach in the 3d space {q1,q2,q3}
  -- realistically, I'm going to call this zero, since real data has no approach, and the dirac distribution has zero skew
  | q1 == q3 = 0
  -- if `q1 == q2`, then skew simplifies to `(q3 - q1) / (q3 - q1)`, or `1`
  | q2 == q1 = 1
  -- similarly, if `q2 == q3`, then skew simplifies to `-1`
  | q2 == q3 = -1
  -- I'm not sure if either of those situations are reliable indicators of skew, though
  | otherwise = (q3 + q1 - 2*q2) `div` (q3 - q1)
