-- author: tm8st (tm8st@hotmail.co.jp)
-- test runner.

module Main where

import TestHGI
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

main = do
  putStr "prop_vector3_dot_size: " >> myQuickCheck prop_vector3_dot_size
  putStr "prop_vector3_normal_size: " >> myQuickCheck prop_vector3_normal_size
  putStr "prop_vector3_divByScalar: " >> myQuickCheck prop_vector3_divByScalar
  putStr "prop_vector3_mulByScalar: " >> myQuickCheck prop_vector3_mulByScalar
  putStrLn "all test finished."
