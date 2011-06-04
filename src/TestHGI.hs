module TestHGI where

import HGI

import Data.Maybe
import System.IO
import Test.QuickCheck
import Test.QuickCheck.Test

{-| Vector3 tests.
-}
instance Arbitrary Vector3 where
  arbitrary = do
      x <- choose (-10000.0, 10000.0)
      y <- choose (-10000.0, 10000.0)
      z <- choose (-10000.0, 10000.0)
      v <- elements [Vector3 x y z]
      return v

prop_vector3_dot_size v = sqrt (v `dot` v) == size v
prop_vector3_normal_size v = size n <= 1.0
  where
    n = case safeNormal v of
          Just nrm -> nrm
          Nothing -> (Vector3 0 0 0)
          
prop_vector3_divByScalar v@(Vector3 x y z) s = v `divByScalar` s == (Vector3 (x/s) (y/s) (z/s))
prop_vector3_mulByScalar v@(Vector3 x y z) s = v `mulByScalar` s == (Vector3 (x*s) (y*s) (z*s))

