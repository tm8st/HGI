-- author: tm8st (tm8st@hotmail.co.jp)
-- test module of HGI.

module TestHGI where

import HGI
import Scene
import Math

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
prop_vector3_normal_size v = abs ((size n) - 1.0) <= 0.001
  where
    n = case safeNormal v of
          Just nrm -> nrm
          Nothing -> (Vector3 0 0 0)

prop_vector3_divByScalar v@(Vector3 x y z) s = v `divByScalar` s == (Vector3 (x/s) (y/s) (z/s))
prop_vector3_mulByScalar v@(Vector3 x y z) s = v `mulByScalar` s == (Vector3 (x*s) (y*s) (z*s))

--
once_prop_vector3_cross = (Vector3 0 1 0) == (Vector3 0 0 1) `cross` (Vector3 1 0 0)
                  && (Vector3 0 0 1) == (Vector3 1 0 0) `cross` (Vector3 0 1 0)

--
once_prop_intersectLinePlane = Just (Vector3 0 0 0) == intersectionLinePlane testLine
                         (Plane (Vector3 0.0 0.0 (-1)) 0.0)
                         && Nothing == intersectionLinePlane testLine
                         (Plane (Vector3 0 0 (-1)) 1)
                         && Nothing == intersectionLinePlane testLine
                         (Plane (Vector3 0 0 (-1)) 1)
                         && Just (Vector3 0 0 5) == intersectionLinePlane testLine
                         (Plane (Vector3 0 0 1) 5)
                         && Nothing == intersectionLinePlane testLine
                         (Plane (Vector3 0 0 1) 11)
  where
    testLine = Line (Vector3 0.0 0.0 0.0) (Vector3 0.0 0.0 10.0)

--
once_prop_intersectionLineTriangle =
  Just ((Vector3 0 0.1 0), (Vector3 0 0 (-1))) == intersectionLineTriangle (Line (Vector3 0.0 0.1 0.0) (Vector3 0.0 0.1 10.0))
                                                (triangleFromPoints (Vector3 (-1) 0 0) (Vector3 0 1 0) (Vector3 1 0 0))
  && 
  Nothing == intersectionLineTriangle (Line (Vector3 0.0 0.0 0.0) (Vector3 0.0 0.0 10.0))
                                      (triangleFromPoints (Vector3 (-1) 0 (-0.1)) (Vector3 0 1 (-0.1)) (Vector3 1 0 (-0.1)))

