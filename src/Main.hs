-- author: tm8st (tm8st@hotmail.co.jp)
-- Global illumination renderer implement in haskell.

{-# LANGUAGE BangPatterns #-}

module Main where

import HGI
import Scene
import Math
import qualified Mesh as Mesh
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
-- defaultWidth = 128
-- defaultHeight = 128
-- defaultWidth = 32
-- defaultHeight = 32

testMaterial = Material { mtDiffuseColor = (Vector3 0.7 0.8 1), mtSpecularPower = 8.0 }
testCamera = Camera { camLocation = (Vector3 0 0 (-10.0)) }
testSphere = ObjectSphere Sphere { sphereCenter =  (Vector3 0 0 5), sphereRadius = 20.0 } testMaterial
testSphere2 = ObjectSphere Sphere { sphereCenter =  (Vector3 30 20 5), sphereRadius = 18.0 } testMaterial
testSphere3 = ObjectSphere Sphere { sphereCenter =  (Vector3 (-30) 20 5), sphereRadius = 18.0 } testMaterial
testSphere4 = ObjectSphere Sphere { sphereCenter =  (Vector3 30 (-20) 5), sphereRadius = 18.0 } testMaterial
testSphere5 = ObjectSphere Sphere { sphereCenter =  (Vector3 (-30) (-20) 5), sphereRadius = 18.0 } testMaterial
testSphere6 = ObjectSphere Sphere { sphereCenter =  (Vector3 0 30 5), sphereRadius = 18.0 } testMaterial
testSphere7 = ObjectSphere Sphere { sphereCenter =  (Vector3 0 (-30) 5), sphereRadius = 18.0 } testMaterial
testPointLight = PointLight{ plRadius = 64, plLocation = (camLocation testCamera), plColor = Vector3 1 1 0.8 }
testPointLight2 = PointLight{ plRadius = 64, plLocation = (Vector3 0 (-60) (-10)), plColor = Vector3 1 0.3 0.3 }
testPointLight3 = PointLight{ plRadius = 64, plLocation = (Vector3 0 60 (-10)), plColor = Vector3 0.4 0.0 1.0 }

-- testScene = Scene { objects = [testSphere, testSphere2, testSphere3
--                               , testSphere4, testSphere5, testSphere6, testSphere7
--                               ]
--               , lights = [testPointLight, testPointLight2, testPointLight3]
--               , camera = testCamera
--               , backgroundColor = Color 0 0 0.5
--               }

colorsToWordsList :: [Color] -> [Word8]
colorsToWordsList = concat . map colorToWord8s

-- | run raytracing.
main :: IO ()
main = do
  objtxt <- readFile "../resource/cube.obj"
  -- objtxt <- readFile "../resource/bunny-res4.obj"
  startMeshLoadTime <- getClockTime
  let (Right !mesh) = Mesh.objMeshFromFileContent objtxt
  endMeshLoadTime <- getClockTime
  print $ timeDiffToString $ diffClockTimes endMeshLoadTime startMeshLoadTime
  putStrLn "mesh loaded."
  args <- getArgs
  let (w,h) = if (length args /= 2)
              then (defaultWidth, defaultHeight)
              else (read (head args) :: Int,
                    read (head (tail args)) :: Int)
      testScene = Scene { objects = [ObjectMesh mesh testMaterial
                                    -- , testSphere2, testSphere3
                                    -- , testSphere4, testSphere5, testSphere6, testSphere7
                                    ]
                        , lights = [testPointLight, testPointLight2, testPointLight3]
                        , camera = testCamera
                        , backgroundColor = Color 0 0 0.5
                        }
  -- putStrLn $ "RayTrace width = " ++ show w ++ ", height = " ++ show h
  startRenderImageTime <- getClockTime
  BMP.writeBMP "result.bmp"
    $ BMP.packRGBA32ToBMP w h
    $ B.pack
    $ colorsToWordsList
 -- $ imgData $ negaposi $ Image w h
 -- $ imgData $ contrast $ Image w h
    $ renderImage (w, h) testScene
  endTime <- getClockTime
  putStrLn "done."
  print $ timeDiffToString $ diffClockTimes endTime startRenderImageTime
