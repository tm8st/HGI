module Main where

import TestHGI
import Test.QuickCheck
import Test.QuickCheck.Test

main = do
putStr "prop_vector3_dot_size: " >> Test.QuickCheck.quickCheck prop_vector3_dot_size
putStr "prop_vector3_normal_size: " >> Test.QuickCheck.quickCheck prop_vector3_normal_size
putStr "prop_vector3_divByScalar: " >> Test.QuickCheck.quickCheck prop_vector3_divByScalar
putStr "prop_vector3_mulByScalar: " >> Test.QuickCheck.quickCheck prop_vector3_mulByScalar
