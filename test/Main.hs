module Main where

import Data.List (sort)
import Data.Maybe (isJust)
import Data.Primitive.Contiguous (SmallArray)
import Statistics.Array.Types (AscList,Quartiles(..))
import Test.Tasty (defaultMain,testGroup)
import Test.Tasty.HUnit (testCase,(@=?))
import Test.Tasty.QuickCheck ((===),(==>))
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Primitive.Contiguous as Arr
import qualified Statistics.Array as Stats
import qualified Statistics.Array.Types as Asc


main :: IO ()
main = defaultMain $ testGroup "stats"
  [ testGroup "quartiles"
    [ testCase "empty" $
      let arr = Arr.empty :: SmallArray Int
       in (Stats.quartiles <$> Asc.fromArray arr) @=? Nothing
    , testProperty "singleton" $ \x ->
      let arr = Arr.singleton x :: SmallArray Int
       in (Stats.quartiles <$> Asc.fromArray arr) === Just (Quartiles x x x)
    , testProperty "doubleton" $ \x0 y0 ->
      let (x, y) = if x0 <= y0 then (x0,y0) else (y0,x0)
          arr = Arr.doubleton x y :: SmallArray Int
       in (Stats.quartiles <$> Asc.fromArray arr) === Just (Quartiles x y y)
    , testProperty "tripleton" $ \x0 y0 z0 ->
      let (x1, y1) = if x0 <= y0 then (x0,y0) else (y0,x0)
          (x,y,z) = if z0 < x1 then (z0,x1,y1) else
                    if z0 < y1 then (x1,z0,y1) else
                      (x1,y1,z0)
          arr = Arr.tripleton x y z :: SmallArray Int
       in (Stats.quartiles <$> Asc.fromArray arr) === Just (Quartiles x y z)
    , testCase "bigun" $
      let arr = Arr.fromList nineDatums :: SmallArray Int
       in (Stats.quartiles <$> Asc.fromArray arr) @=? Just (Quartiles 3 4 8)
    ]
  , testProperty "median is second quartile" $ \xs ->
      let asc = Asc.fromList xs :: Maybe (AscList SmallArray Int)
       in isJust asc ==>
          (Stats.median <$> asc) === (q2 . Stats.quartiles <$> asc)
  , testGroup "bowley skew" $
    [ testCase "smoke" $
      let arr = Arr.fromList nineDatums :: SmallArray Int
          qs = Stats.quartiles <$> Asc.fromArray arr
       in (Stats.bowleySkew <$> qs) @=? Just 0.6
    , testProperty "outlier" $ \x ->
      let arr = Arr.fromList (x:eightDatums) :: SmallArray Int
          qs = Stats.quartiles <$> Asc.fromArray arr
       in x > 9 ==>
          (Stats.bowleySkew <$> qs) === Just 0.6
    ]
  , testGroup "median of absolute deviations" $
    [ testCase "smoke" $
      let arr = Arr.fromList nineDatums :: SmallArray Int
       in (Stats.mad <$> Asc.fromArray arr) @=? Just 2
    ]
  , testGroup "list derivative" $
    [ testProperty "like naive" $ \unsortedXs ->
      let xs = sort unsortedXs
          lst' = zipWith (-) (tail xs) xs
          asc = Asc.unsafeFromAscendingList xs :: AscList SmallArray Int
       in (not . null) xs ==>
          (Stats.listDerivative asc) === (Arr.fromList lst' :: SmallArray Int)
    ]
  ]


eightDatums :: [Int]
-- 0,2,3,3,4,6,8,9
eightDatums = [3,6,8,2,4,0,3,9]

nineDatums :: [Int]
-- 0,2,3,3,4,6,8,9,54
nineDatums = [3,6,8,2,4,0,3,54,9]
