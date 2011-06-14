-- author: tm8st (tm8st@hotmail.co.jp)
-- math function and types.

{-# LANGUAGE BangPatterns #-}

module Math where

import Data.Word (Word8)

-- | simple 3D vector.
data Vector3 = Vector3 { vX :: Double
                       , vY :: Double
                       , vZ :: Double }
               deriving(Eq, Show)

instance Num Vector3 where
  (+) (Vector3 lx ly lz) (Vector3 rx ry rz) = Vector3 (lx + rx) (ly + ry) (lz + rz)
  (-) (Vector3 lx ly lz) (Vector3 rx ry rz) = Vector3 (lx - rx) (ly - ry) (lz - rz)
  (*) (Vector3 lx ly lz) (Vector3 rx ry rz) = Vector3 (lx * rx) (ly * ry) (lz * rz)
  negate (Vector3 x y z) = Vector3 (-x) (-y) (-z)
  abs (Vector3 x y z) = Vector3 (abs x) (abs y) (abs z)
  signum (Vector3 x y z) = Vector3 (signum x) (signum y) (signum z)
  fromInteger l = Vector3 d d d
   where d = fromInteger l 

dot :: Vector3 -> Vector3 -> Double
(Vector3 lx ly lz) `dot` (Vector3 rx ry rz) = lx * rx + ly * ry + lz * rz

size :: Vector3 -> Double
size (Vector3 x y z) = sqrt (x*x + y*y + z*z)

cross :: Vector3 -> Vector3 -> Vector3
l `cross` r = Vector3 (vY l * vZ r - vZ l * vY r)
                      (vZ l * vX r - vX l * vZ r)
                      (vX l * vY r - vY l * vX r)

scalarTriple :: Vector3 -> Vector3 -> Vector3 -> Double
scalarTriple u v w = (cross u v) `dot` w

divByScalar :: Vector3 -> Double -> Vector3
-- v `divByScalar` s = v `mulByScalar` invS
--   where invS = 1.0 / s
(Vector3 x y z) `divByScalar` s = Vector3 (x / s) (y / s) (z / s)


mulByScalar :: Vector3 -> Double -> Vector3
(Vector3 x y z) `mulByScalar` s = Vector3 (x * s) (y * s) (z * s)

normal :: Vector3 -> Vector3
normal v = v `divByScalar` size v

safeNormal :: Vector3 -> Maybe Vector3
safeNormal v = let len = size v
               in if len /= 0.0
               then Just $ v `divByScalar` len
               else Nothing

-- | Ray, use for intersection.
data Ray = Ray { rayStart :: Vector3
               , rayDirection :: Vector3
               }
           deriving(Eq, Show)

-- | Sphere, use for intersection.
data Sphere = Sphere { sphereCenter :: Vector3
                     , sphereRadius :: Double
                     }
              deriving(Eq, Show)

-- | Color.
data Color = Color { cR :: Double
                   , cG :: Double
                   , cB :: Double }
             deriving(Eq, Show)

instance Num Color where
  (+) (Color lx ly lz) (Color rx ry rz) = Color (lx + rx) (ly + ry) (lz + rz)
  (-) (Color lx ly lz) (Color rx ry rz) = Color (lx - rx) (ly - ry) (lz - rz)
  (*) (Color lx ly lz) (Color rx ry rz) = Color (lx * rx) (ly * ry) (lz * rz)
  negate (Color x y z) = Color (-x) (-y) (-z)
  abs (Color x y z) = Color (abs x) (abs y) (abs z)
  signum (Color x y z) = Color (signum x) (signum y) (signum z)
  fromInteger l = Color d d d
   where d = fromInteger l 

cdot :: Color -> Color -> Double
(Color lx ly lz) `cdot` (Color rx ry rz) = lx * rx + ly * ry + lz * rz

csize :: Color -> Double
csize (Color x y z) = sqrt (x*x + y*y + z*z)

cmulByScalar :: Color -> Double -> Color
(Color x y z) `cmulByScalar` s = Color (x * s) (y * s) (z * s)

fromVector3 :: Vector3 -> Color
fromVector3 (Vector3 x y z) = Color x y z

colorToWord8s :: Color -> [Word8]
colorToWord8s c = (map doubleColorValueToWord8 [cR c, cG c, cB c, 1.0])
  where
    doubleColorValueToWord8 v = max 0 $ min 255 (truncate (v * 255))

-- Plane
data Plane = Plane { planeNormal :: Vector3
                   , planeW :: Double
                   }
             deriving(Eq, Show)

planeFromNormalDistance n d = Plane n d

-- Triangle
data Triangle = Triangle Vector3 Vector3 Vector3
                deriving(Eq, Show)

barycentricPosition :: Triangle -> (Double, Double, Double) -> Vector3
barycentricPosition (Triangle v0 v1 v2) (u, v, w) =
  v0 `mulByScalar` u + v1 `mulByScalar` v + v2 `mulByScalar` w

triangleNormal :: Triangle -> Vector3
triangleNormal (Triangle u v w) =
  let uv = v - u
      uw = w - u
  in normal $ cross uv uw

data Line = Line Vector3 Vector3
          deriving(Eq, Show)

testRay = Ray (Vector3 0.0 0.0 0.0) (Vector3 0.0 0.0 (1.0))
testLine = Line (Vector3 0.0 0.0 0.0) (Vector3 0.0 0.0 5.0)
testPlane = Plane (Vector3 0.0 0.0 (1.0)) (3.0)
testTriangle = Triangle (Vector3 0.0 1.0 3.0)
                        (Vector3 (-1.0) (-1.0) 3.0)
                        (Vector3 1.0 0.0 3.0)

intersectionLinePlane :: Line -> Plane -> Maybe Vector3
intersectionLinePlane (Line s e) (Plane pn pd) = 
  let d = e - s
      t = (pd - (pn `dot` s)) / (pn `dot` d)
  in if (0.0 <= t) && (t <= 1.0)
     then Just $ s + d `mulByScalar` t
     else Nothing

intersectionLineTriangle :: Line -> Triangle -> Maybe (Vector3, Vector3)
intersectionLineTriangle (Line p q) tri@(Triangle a b c) = 
  let pq = q - p
      pa = a - p
      pb = b - p
      pc = c - p
      u = scalarTriple pq pc pb
      v = scalarTriple pq pa pc
      w = scalarTriple pq pb pa
  in if (abs u >= epsilon)
     && (abs v >= epsilon)
     && (abs w >= epsilon)
     then let t = 1.0 / (u + v + w)
          in Just (barycentricPosition tri (u*t, v*t, w*t), normal $ triangleNormal tri)
     else Nothing
  where
    epsilon = 0.00001
