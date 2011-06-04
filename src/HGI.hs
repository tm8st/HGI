-- Global illumination renderer implement in haskell.
-- Reference.
-- http://www.t-pot.com/program/92_RayTraceSphere/index.html
-- http://www.t-pot.com/program/94_RayTraceLighting/index.html
-- http://boegel.kejo.be/ELIS/Haskell/HRay/

module HGI where

import Debug.Trace (trace)
import Data.List (foldl')
import Data.Word (Word8)

-- | simple 3D vector.
data Vector3 = Vector3 { vX :: Double
                       , vY :: Double
                       , vZ :: Double }
               deriving(Eq, Show)

instance Num Vector3 where
  (+) l r = Vector3 (vX l + vX r) (vY l + vY r) (vZ l + vZ r)
  (-) l r = Vector3 (vX l - vX r) (vY l - vY r) (vZ l - vZ r)
  (*) l r = Vector3 (vX l * vX r) (vY l * vY r) (vZ l * vZ r)
  negate v = Vector3 (- vX v) (- vY v) (- vZ v)
  abs v = Vector3 (abs $ vX v) (abs $ vY v) (abs $ vZ v)
  signum v = Vector3 (signum $ vX v) (signum $ vY v) (signum $ vZ v)
  fromInteger l = Vector3 d d d
   where d = fromInteger l 

dot :: Vector3 -> Vector3 -> Double
l `dot` r = vX l * vX r + vY l * vY r + vZ l * vZ r

size :: Vector3 -> Double
size v = sqrt $ dot v v

cross :: Vector3 -> Vector3 -> Vector3
l `cross` r = Vector3 (vY l * vZ r - vZ l * vY r)
                      (vZ l * vX r - vX l * vZ r)
                      (vX l * vY r - vY l * vX r)

divByScalar :: Vector3 -> Double -> Vector3
v `divByScalar` s = Vector3 (vX v / s) (vY v / s) (vZ v / s)

mulByScalar :: Vector3 -> Double -> Vector3
v `mulByScalar` s = Vector3 (vX v * s) (vY v * s) (vZ v * s)

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

fromVector3 :: Vector3 -> Color
fromVector3 v = Color (vX v) (vY v) (vZ v)

colorToWord8s :: Color -> [Word8]
colorToWord8s c = (map doubleColorValueToWord8 [cR c, cG c, cB c]) ++ [255]
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

-- | result of ray trace, 
data TraceResult = TraceResult { trLocation :: Vector3  -- ^ hit location.
                               , trMaterial :: Material -- ^ hit object's material
                               , trNormal  :: Vector3   -- ^ hit location's normal.
                               }
                   deriving(Eq, Show)

-- | Material, define object physical properties.
data Material = Material { mtDiffuseColor :: Vector3
                         , mtSpecularPower :: Double
                         }
                deriving(Eq, Show)

-- | Light.
data Light = PointLight{ plRadius :: Double
                       , plLocation :: Vector3
                       , plColor :: Vector3 
                       }
             deriving (Eq, Show)

-- | Camera.
data Camera = Camera { camLocation :: Vector3
                     }
              deriving(Eq, Show)

-- | Screen Resolution.
type Resolution = (Int, Int)


-- | test scene setting values.
testCamera = Camera { camLocation = (Vector3 0 0 (-20)) }
testSphere = Sphere { sphereCenter =  (Vector3 0 0 5), sphereRadius = 18.0 }
testMaterial = Material { mtDiffuseColor = (Vector3 0.7 0.8 1), mtSpecularPower = 8.0 }
testPointLight = PointLight{ plRadius = 20, plLocation = (camLocation testCamera), plColor = Vector3 1 1 1 }
testBGColor = Color 0 0 0.5

-- | enumrate pixels and cast ray from camera to every pixels.
renderImage :: Resolution -> Camera -> [Word8]
renderImage (w, h) cam = concat [colorToWord8s $ tracePixelRays (x, y) | y <- [0..(h-1)], x <- [0..(w-1)]]
  where
    tracePixelRays (x, y) = {-# SCC "tracePixelRays" #-}
      let pixelPosInScene = screenPosToScenePos x y (w, h)
          camToPixelRay = getPosToPosRay (camLocation cam) pixelPosInScene
      in case rayTrace camToPixelRay testSphere of
        Just traceResult -> shade traceResult cam
        otherwise        -> testBGColor

getPosToPosRay :: Vector3 -> Vector3 -> Ray
getPosToPosRay from to = Ray{ rayStart = from
                            , rayDirection = normal (to - from)
                            }

screenPosToScenePos :: Int -> Int -> Resolution -> Vector3
screenPosToScenePos x y (w, h) = Vector3 ((fromIntegral x - fromIntegral w * 0.5) * 0.1)
                                         ((fromIntegral h * 0.5 - fromIntegral y) * 0.1)
                                         0.0

-- | Ray vs Sphere.
rayTrace :: Ray -> Sphere -> Maybe TraceResult
rayTrace (Ray rs rd) (Sphere sc sr) =
  let m = rs - sc
      b = m `dot` rd
      c = m `dot` m - sr*sr
      discr = b * b - c
      t = -b - sqrt discr
      location = (rs + (rd `mulByScalar` (if t < 0.0 then 0.0 else t)))
  in if (c > 0.0 && b > 0.0)
     || (discr < 0.0)
     then Nothing
     else Just TraceResult { trLocation = location
                           , trMaterial = testMaterial
                           , trNormal   = normal (location - sc)
                           }

-- | calculate ray hit location's color.
shade :: TraceResult -> Camera -> Color
shade tr cam = resultColor
  where
    trLoc = trLocation tr
    trN = trNormal tr
    trMat = trMaterial tr
    lightDir = normal $ (plLocation testPointLight) - trLoc
    diffuse = lambert trN lightDir
    specular = halfVectorSpecular (normal ((camLocation cam) - trLoc)) trN lightDir (mtSpecularPower trMat)
    diffuseColor = (plColor testPointLight) * (mtDiffuseColor trMat) `mulByScalar` diffuse
    specularColor = (plColor testPointLight `mulByScalar` specular)
    attenuation = pointLightAttenuation trLoc (plLocation testPointLight) (plRadius testPointLight)
    resultColor = fromVector3 $ (diffuseColor + specularColor) `mulByScalar` attenuation

-- | distance attenuation.
pointLightAttenuation :: Vector3 -> Vector3 -> Double -> Double
pointLightAttenuation pos lightPos lightRadius = max 0.0 $ 1.0 - (size (lightPos - pos) / lightRadius)

lambert :: Vector3 -> Vector3 -> Double
lambert n l = max 0.0 (n `dot` l)

halfLambert :: Vector3 -> Vector3 -> Double
halfLambert n l = (n `dot` l) * 0.5 + 0.5

halfVectorSpecular :: Vector3 -> Vector3 -> Vector3 -> Double -> Double
halfVectorSpecular e n l specularPower =
  let h = normal ((negate e) + l)
  in max 0.0 ((h `dot` n) ** specularPower)
