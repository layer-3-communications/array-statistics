{-# LANGUAGE BangPatterns #-}

import Gauge.Main (defaultMain, bench, whnf)

import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Data.Primitive.Contiguous (PrimArray, fromListN)
import Statistics.Array.Types (AscList)
import System.Random (random, mkStdGen)

import qualified Statistics.Array as Stats
import qualified Statistics.Array.Types as Asc

main :: IO ()
main = defaultMain
  [ bench "sort-16" $ whnf (fromJust . Asc.fromArray) input16
  , bench "sort-128" $ whnf (fromJust . Asc.fromArray) input128
  , bench "sort-1024" $ whnf (fromJust . Asc.fromArray) input1024
  , bench "sort-65536" $ whnf (fromJust . Asc.fromArray) input65536
  , bench "list-derivative" $ whnf Stats.listDerivative asc1024
  , bench "median-of-absolute-deviations" $ whnf Stats.mad asc1024
  ]
  where
  input16, input128, input1024, input65536 :: PrimArray Int
  !input16 = fromListN 16 $ take 16 . drop 0 $ inputInf
  !input128 = fromListN 128 $ take 128 . drop 16 $ inputInf
  !input1024 = fromListN 1024 $ take 1024 . drop (16+128) $ inputInf
  !input65536 = fromListN 65536 $ take 65536 . drop (16+128+1024) $ inputInf
  asc1024 :: AscList PrimArray Int
  asc1024 = fromJust $ Asc.fromArray input1024

inputInf :: [Int]
inputInf = unfoldr (Just . random) seed
  where
  seed = mkStdGen 1234567
