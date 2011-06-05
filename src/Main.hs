-- Global illumination renderer implement in haskell.

module Main where

import HGI
import System.Time (getClockTime, diffClockTimes, timeDiffToString)
import System.Environment (getArgs)
import qualified Data.ByteString as B
import qualified Codec.BMP as BMP

defaultWidth = 640
defaultHeight = 640

-- | run raytracing.
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
    $ renderImage (w, h) testCamera
  endTime <- getClockTime
  putStrLn "done."
  print $ timeDiffToString $ diffClockTimes endTime startTime
