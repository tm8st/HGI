-- Global illumination renderer implement in haskell.

module Main where

import HGI
import Scene
import Math
import System.Time (getClockTime, diffClockTimes, timeDiffToString)
import System.Environment (getArgs)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Codec.BMP as BMP

-- defaultWidth = 1280
-- defaultHeight = 1280
defaultWidth = 640
defaultHeight = 640
-- defaultWidth = 320
-- defaultHeight = 320

testMaterial = Material { mtDiffuseColor = (Vector3 0.7 0.8 1), mtSpecularPower = 8.0 }
testCamera = Camera { camLocation = (Vector3 0 0 (-20)) }
testSphere = Object Sphere { sphereCenter =  (Vector3 0 0 5), sphereRadius = 20.0 } testMaterial
testSphere2 = Object Sphere { sphereCenter =  (Vector3 30 20 5), sphereRadius = 18.0 } testMaterial
testSphere3 = Object Sphere { sphereCenter =  (Vector3 (-30) 20 5), sphereRadius = 18.0 } testMaterial
testSphere4 = Object Sphere { sphereCenter =  (Vector3 30 (-20) 5), sphereRadius = 18.0 } testMaterial
testSphere5 = Object Sphere { sphereCenter =  (Vector3 (-30) (-20) 5), sphereRadius = 18.0 } testMaterial
testSphere6 = Object Sphere { sphereCenter =  (Vector3 0 30 5), sphereRadius = 18.0 } testMaterial
testSphere7 = Object Sphere { sphereCenter =  (Vector3 0 (-30) 5), sphereRadius = 18.0 } testMaterial
testPointLight = PointLight{ plRadius = 64, plLocation = (camLocation testCamera), plColor = Vector3 1 1 0.8 }
testPointLight2 = PointLight{ plRadius = 64, plLocation = (Vector3 0 (-60) (-10)), plColor = Vector3 1 0.3 0.3 }
testPointLight3 = PointLight{ plRadius = 64, plLocation = (Vector3 0 60 (-10)), plColor = Vector3 0.4 0.0 1.0 }

testScene = Scene { objects = [testSphere, testSphere2, testSphere3,
                               testSphere4, testSphere5, testSphere6, testSphere7]
              , lights = [testPointLight, testPointLight2, testPointLight3]
              , camera = testCamera
              , backgroundColor = Color 0 0 0.5
              }

colorsToWordsList :: [Color] -> [Word8]
colorsToWordsList = concat . map colorToWord8s

-- | run raytracing.
main :: IO ()
main = do
  args <- getArgs
  let (w,h) = if (length args /= 2)
              then (defaultWidth, defaultHeight)
              else (read (head args) :: Int,
                    read (head (tail args)) :: Int)
  putStrLn $ "RayTrace width = " ++ show w ++ ", height = " ++ show h
  startTime <- getClockTime
  BMP.writeBMP "result.bmp"
    $ BMP.packRGBA32ToBMP w h
    $ B.pack
    $ colorsToWordsList
    $ imgData $ negaposi $ Image w h
 -- $ imgData $ contrast $ Image w h
    $ renderImage (w, h) testScene
  endTime <- getClockTime
  putStrLn "done."
  print $ timeDiffToString $ diffClockTimes endTime startTime

data Image = Image { imgWidth :: Int
                   , imgHeight :: Int
                   , imgData :: [Color]
                   }

type Filter = Image -> Image

-- morphologicalAA :: Filter

monotone :: Filter
monotone img = img{ imgData = map f (imgData img) }
  where
    f c = let p = min 1.0 $ csize c
          in Color p p p

contrast :: Filter
contrast img = img{ imgData = map f (imgData img) }
  where
    f c = let p = min 1.0 $ csize c
              p' = p*p
          in c `cmulByScalar` p'

negaposi :: Filter
negaposi img = img{ imgData = map f (imgData img) }
  where
    f c = negate c
