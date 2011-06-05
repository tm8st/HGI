{-# LANGUAGE BangPatterns #-}

module Math where

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

divByScalar :: Vector3 -> Double -> Vector3
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

