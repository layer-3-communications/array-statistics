{-# LANGUAGE BangPatterns #-}

import Gauge.Main (defaultMain, bench, whnf)

import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Data.Primitive.Contiguous (PrimArray, fromListN)
import Statistics.Array.Types (AscList)
import System.Random (random, mkStdGen)

import qualified Data.Primitive as PM
import qualified Statistics.Array as Stats
import qualified Statistics.Array.Types as Asc

main :: IO ()
main = defaultMain
  [ bench "sort-16" $ whnf fromAscInt input16
  , bench "sort-128" $ whnf fromAscInt input128
  , bench "sort-1024" $ whnf fromAscInt input1024
  , bench "sort-65536" $ whnf fromAscInt input65536
  , bench "list-derivative" $ whnf performListDerivativeInt asc1024
  , bench "median-of-absolute-deviations" $ whnf Stats.mad asc1024
  ]
  where
  input16, input128, input1024, input65536 :: PrimArray Int64
  !input16 = fromListN 16 $ take 16 . drop 0 $ inputInf
  !input128 = fromListN 128 $ take 128 . drop 16 $ inputInf
  !input1024 = fromListN 1024 $ take 1024 . drop (16+128) $ inputInf
  !input65536 = fromListN 65536 $ take 65536 . drop (16+128+1024) $ inputInf
  asc1024 :: AscList Int64
  asc1024 = fromJust $ Asc.fromArray input1024

fromAscInt :: PrimArray Int64 -> Int64
{-# noinline fromAscInt #-}
fromAscInt !x = case Asc.fromArray x of
  Nothing -> 0
  Just (Asc.AscList y) -> fromIntegral (PM.sizeofPrimArray y)

-- This is here to make it easy to inspect Core to confirm that specialization
-- works correctly. 
performListDerivativeInt :: AscList Int64 -> PrimArray Int64
{-# noinline performListDerivativeInt #-}
performListDerivativeInt !x = Stats.listDerivative x

inputInf :: [Int64]
inputInf = unfoldr (Just . random) seed
  where
  seed = mkStdGen 1234567
