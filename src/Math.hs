-- author: tm8st (tm8st@hotmail.co.jp)
-- math function and types.

module Math where

import Debug.Trace (trace, traceShow)
import Data.Word (Word8)

data Vector4 = Vector4 Double Double Double Double
               deriving (Eq, Show)

dotv4 :: Vector4 -> Vector4 -> Double
(Vector4 lx ly lz lw) `dotv4` (Vector4 rx ry rz rw) = lx * rx + ly * ry + lz * rz + lw * rw

-- | simple 4x4 matrix.
data Matrix = Matrix [Vector3]
            deriving (Eq, Show)

matrixAxis :: Matrix -> Int -> Vector4
matrixAxis m i = undefined

matrixTransform :: Matrix -> Vector3 -> Vector4
matrixTransform m v = undefined

-- define Num functions.
instance Num Matrix where
  (+) (Matrix l) (Matrix r) = Matrix $ zipWith (+) l r
  (-) (Matrix l) (Matrix r) = Matrix $ zipWith (-) l r
  (*) l r = undefined
  negate (Matrix m) = Matrix $ map negate m
  abs (Matrix m) = Matrix $ map abs m
  signum (Matrix m) = undefined
  fromInteger i = undefined

-- | simple 3D vector.
data Vector3 = Vector3 { vX :: Double
                       , vY :: Double
                       , vZ :: Double
                       }
               deriving(Eq, Show)

-- define Num functions.
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
(Vector3 x y z) `divByScalar` s = Vector3 (x / s) (y / s) (z / s)
-- v `divByScalar` s = v `mulByScalar` invS
--   where invS = 1.0 / s

mulByScalar :: Vector3 -> Double -> Vector3
(Vector3 x y z) `mulByScalar` s = Vector3 (x * s) (y * s) (z * s)

normal :: Vector3 -> Vector3
normal v = v `divByScalar` size v

safeNormal :: Vector3 -> Maybe Vector3
safeNormal v = let len = size v
               in if len /= 0.0
               then Just $ v `divByScalar` len
               else Nothing

-- | Color.
data Color = Color { cR :: Double
                   , cG :: Double
                   , cB :: Double }
             deriving(Eq, Show)

-- define Num functions.
instance Num Color where
  (+) (Color lx ly lz) (Color rx ry rz) = Color (lx + rx) (ly + ry) (lz + rz)
  (-) (Color lx ly lz) (Color rx ry rz) = Color (lx - rx) (ly - ry) (lz - rz)
  (*) (Color lx ly lz) (Color rx ry rz) = Color (lx * rx) (ly * ry) (lz * rz)
  negate (Color x y z) = Color (-x) (-y) (-z)
  abs (Color x y z) = Color (abs x) (abs y) (abs z)
  signum (Color x y z) = Color (signum x) (signum y) (signum z)
  fromInteger l = Color d d d
   where d = fromInteger l 

-- @TODO Vector3と共通化 class? newtype?
cdot :: Color -> Color -> Double
(Color lx ly lz) `cdot` (Color rx ry rz) = lx * rx + ly * ry + lz * rz

csize :: Color -> Double
csize (Color x y z) = sqrt (x*x + y*y + z*z)

cmulByScalar :: Color -> Double -> Color
(Color x y z) `cmulByScalar` s = Color (x * s) (y * s) (z * s)

fromVector3 :: Vector3 -> Color
fromVector3 (Vector3 x y z) = Color x y z

-- | convert color to Word8
colorToWord8s :: Color -> [Word8]
colorToWord8s c = (map doubleColorValueToWord8 [cR c, cG c, cB c, 1.0])
  where
    doubleColorValueToWord8 v = max 0 $ min 255 (truncate (v * 255))

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

-- | Plane
data Plane = Plane { planeNormal :: Vector3
                   , planeW :: Double
                   }
             deriving(Eq, Show)

distancePointPlane :: Vector3 -> Plane -> Double
distancePointPlane v (Plane pn pd) = v `dot` pn - pd

planeFromNormalDistance :: Vector3 -> Double -> Plane
planeFromNormalDistance n d = Plane n d

-- | Triangle v0 v1 v2 n
data Triangle = Triangle { triVertex0 :: Vector3
                         , triVertex1 :: Vector3
                         , triVertex2 :: Vector3
                         , triNormal :: Vector3
                         }
                deriving(Eq, Show)
-- | 
triVertecies :: Triangle -> [Vector3]
triVertecies (Triangle a b c _) = [a, b, c]

-- |
triangleEdge :: Triangle -> Int -> Line
triangleEdge t 0 = Line (triVertex2 t) (triVertex0 t)
triangleEdge t 1 = Line (triVertex0 t) (triVertex1 t)
triangleEdge t 2 = Line (triVertex1 t) (triVertex2 t)

-- |
triangleFromPoints :: Vector3 -> Vector3 -> Vector3 -> Triangle
triangleFromPoints a b c = Triangle a b c n
  where
    ab = b - a
    ac = c - a
    n = normal $ cross ab ac

-- |
trianglesFromPointList :: [Vector3] -> [Triangle]
trianglesFromPointList xs = step xs
  where
    step (a:b:c:[]) = [(triangleFromPoints a b c)]
    step (a:b:c:d:[]) = [(triangleFromPoints a b c), (triangleFromPoints c d a)]
    step [] = []

-- | calc triangle barycentric position.
barycentricPosition :: Triangle -> (Double, Double, Double) -> Vector3
barycentricPosition (Triangle v0 v1 v2 n) (u, v, w) =
  v0 `mulByScalar` u + v1 `mulByScalar` v + v2 `mulByScalar` w

-- | Line
data Line = Line Vector3 Vector3
            deriving(Eq, Show)

-- | intersection Line vs Plane, return HitLocation.
intersectionLinePlane :: Line -> Plane -> Maybe Vector3
intersectionLinePlane (Line s e) (Plane pn pd) = 
  let d = e - s
      t = (pd - (pn `dot` s)) / (pn `dot` d)
  in if (0.0 <= t) && (t <= 1.0)
     then Just $ s + d `mulByScalar` t
     else Nothing

-- | intersection Line vs Triangle, return (HitLocation, HitNormal).
intersectionLineTriangle :: Line -> Triangle -> Maybe (Vector3, Vector3)
intersectionLineTriangle (Line p q) tri@(Triangle a b c n) =
  ret
  -- trace ("" ++ (show u) ++ ", " ++ (show v) ++ ", " ++ show w) ret
  where
    epsilon = 0.0000000000001
    pq = q - p
    pa = a - p
    pb = b - p
    pc = c - p
    u = scalarTriple pq pc pb
    v = scalarTriple pq pa pc
    w = scalarTriple pq pb pa
    ret = if (dot n $ normal pq) <= 0.000
          && (u >= epsilon)
          && (v >= epsilon)
          && (w >= epsilon)
          then let t = 1.0 / (u + v + w)
               in Just (barycentricPosition tri (u*t, v*t, w*t), n)
          else Nothing
