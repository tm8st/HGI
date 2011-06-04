-- Global illumination renderer implement in haskell.
-- Reference.
-- http://www.t-pot.com/program/92_RayTraceSphere/index.html
-- http://www.t-pot.com/program/94_RayTraceLighting/index.html
-- http://boegel.kejo.be/ELIS/Haskell/HRay/

module Main where

import HGI
import System.Time (getClockTime, diffClockTimes, timeDiffToString)
import System.Environment (getArgs)
import qualified Data.ByteString as B
import qualified Codec.BMP as BMP

testWidth = 640
testHeight = 640

-- | run raytracing.
main = do
  args <- getArgs
  let (w,h) = if (length args /= 2)
              then (testWidth, testHeight)
              else (read (head args) :: Int,
                    read (head (tail args)) :: Int)
  putStrLn $ "RayTrace width = " ++ show w ++ ", height = " ++ show h
  startTime <- getClockTime
  BMP.writeBMP "result.bmp"
    $ BMP.packRGBA32ToBMP w h
    $ B.pack
    $ renderImage (w, h) testCamera
  endTime <- getClockTime
  putStrLn "done."
  print $ timeDiffToString $ diffClockTimes endTime startTime
