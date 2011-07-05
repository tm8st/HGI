-- author: tm8st (tm8st@hotmail.co.jp)
-- test runner.

module Main where

import TestHGI
import BSP
import Test.QuickCheck
import Test.QuickCheck.Test
import System.Cmd
import System.Exit

-- added se notify.
myQuickCheck p =
  do 
    ret <- Test.QuickCheck.quickCheckResult p
    if isSuccess ret
    then return ()
    else do 
      exitCode <- system "afplay \"../resource/b_095.mp3\""
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure e -> putStrLn $ "notify failed. exit code is " ++ show e

-- added se notify.
myQuickCheckOnce p =
  do
    if p
    then do 
      putStrLn "+++ OK, passed test."
      return ()
    else do 
      putStrLn "failed..."
      exitCode <- system "afplay \"../resource/b_095.mp3\""
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure e -> putStrLn $ "notify failed. exit code is " ++ show e

main = do
  putStr "prop_vector3_dot_size: " >> myQuickCheck prop_vector3_dot_size
  putStr "prop_vector3_normal_size: " >> myQuickCheck prop_vector3_normal_size
  putStr "prop_vector3_divByScalar: " >> myQuickCheck prop_vector3_divByScalar
  putStr "prop_vector3_mulByScalar: " >> myQuickCheck prop_vector3_mulByScalar
  putStr "once_prop_vector3_cross: " >> myQuickCheckOnce once_prop_vector3_cross
  putStr "once_prop_intersectLinePlane: " >> myQuickCheckOnce once_prop_intersectLinePlane
  putStr "once_prop_intersectionLineTriangle: " >> myQuickCheckOnce once_prop_intersectionLineTriangle
  putStr "once_prop_classifyPointToPlane: " >> myQuickCheckOnce once_prop_classifyPointToPlane
  putStr "once_prop_classifyTriangleToPlane: " >> myQuickCheckOnce once_prop_classifyTriangleToPlane
  putStr "once_prop_splitTriangleByPlane: " >> myQuickCheckOnce once_prop_splitTriangleByPlane
  putStrLn "all test finished."
