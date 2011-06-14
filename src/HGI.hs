-- author: tm8st (tm8st@hotmail.co.jp)
-- Global illumination renderer implement in haskell.

module HGI where

import Debug.Trace (trace)
import Data.List (foldl', sortBy)
import Data.Word (Word8)
import Scene
import Math
import Mesh

-- | result of ray trace, 
data TraceResult = TraceResult { trLocation :: Vector3  -- ^ hit location.
                               , trMaterial :: Material -- ^ hit object's material
                               , trNormal  :: Vector3   -- ^ hit location's normal.
                               , trObject :: Object -- ^ hit object
                               }
                   deriving(Eq, Show)

-- | Screen Resolution.
type Resolution = (Int, Int)
type DoubleResolution = (Double, Double)

-- | enumrate pixels and cast ray from camera to every pixels.
renderImage :: Resolution -> Scene -> [Color]
renderImage (w, h) scene@(Scene objs lgts cam _) = 
  [tracePixelRays (x, y) | y <- [0..(h-1)], x <- [0..(w-1)]]
  where
    fres = (fromIntegral w, fromIntegral h)
    tracePixelRays (x, y) = {-# SCC "tracePixelRays" #-}
      let pixelPosInScene = screenPosToScenePos x y fres
          camToPixelRay = getPosToPosRay (camLocation cam) pixelPosInScene
          ret = findNearestRayHitLocation scene camToPixelRay
      in case ret of
        Just traceResult -> shade traceResult scene
        otherwise        -> backgroundColor scene

getPosToPosRay :: Vector3 -> Vector3 -> Ray
getPosToPosRay from to = Ray{ rayStart = from
                            , rayDirection = normal (to - from)
                            }

screenPosToScenePos :: Int -> Int -> DoubleResolution -> Vector3
screenPosToScenePos x y (w, h) = Vector3 ((fromIntegral x - w * 0.5) * 0.1)
                                         ((h * 0.5 - fromIntegral y) * 0.1)
                                         0.0

-- |
findNearestRayHitLocation :: Scene -> Ray -> Maybe TraceResult
findNearestRayHitLocation scene ray =
  let m = foldl' collectResult [] $ objects scene
  in if null m
  then Nothing
  else Just $ fst (head $ sortBy distanceComp m)
  where
    camLoc = camLocation $ camera scene
    distanceComp (_, ld) (_, rd) = ld `compare` rd
    collectResult acc obj = case rayTrace ray obj of
                  Just tr -> [(tr, size $ (trLocation tr) - camLoc)] ++ acc
                  Nothing -> acc
  
-- | Ray vs Object.
rayTrace :: Ray -> Object -> Maybe TraceResult

-- | Ray vs Sphere
rayTrace (Ray rs rd) obj@(ObjectSphere (Sphere sc sr) material) =
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
                           , trMaterial = material
                           , trNormal   = normal (location - sc)
                           , trObject   = obj
                           }
-- | Ray vs Mesh
rayTrace (Ray rs rd) obj@(ObjectMesh mesh material) =
  let hits = foldl' findHitLocation [] (triangles mesh)
  in if null hits
     then Nothing
     else Just TraceResult { trLocation = (fst $ head hits)
                           , trMaterial = material
                           , trNormal   = (snd $ head hits)
                           , trObject   = obj
                           }
  where
    testLine = Line (rs) (rs + rd `mulByScalar` 10000.0)
    findHitLocation acc tri = 
      case intersectionLineTriangle testLine tri of
        Just (v, n) -> acc ++ [(v,n)]
        Nothing -> acc

-- | calculate ray hit location's color.
shade :: TraceResult -> Scene -> Color
shade tr scene =
    foldl' collectDirectIllumination (Color 0 0 0) (lights scene)
  where
    collectDirectIllumination acc light = acc + shadeByDirectIllumination tr light scene

shadeByDirectIllumination :: TraceResult -> Light -> Scene -> Color
shadeByDirectIllumination tr (PointLight { plLocation = plLocation, plRadius = plRadius, plColor = plColor })
                          scene = resultColor
  where
    camLoc = camLocation $ camera scene
    trLoc = trLocation tr
    trN = trNormal tr
    trMat = trMaterial tr
    lightDir = normal $ plLocation - trLoc
    diffuse = lambert trN lightDir
    specular = halfVectorSpecular (normal (camLoc - trLoc)) trN lightDir (mtSpecularPower trMat)
    diffuseColor = plColor * (mtDiffuseColor trMat) `mulByScalar` diffuse
    specularColor = plColor `mulByScalar` specular
    attenuation = pointLightAttenuation trLoc plLocation plRadius
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
  let h = normal $ (negate e) + l
  in max 0.0 $ (h `dot` n) ** specularPower
