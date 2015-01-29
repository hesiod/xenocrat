{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck.All

import Vector

prop_scalar1 xs = xs == (applyScalar (*) xs 1)

prop_normalize_idempotency xs = (normalize $ normalize xs) == normalize xs

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> if passed then putStrLn "All tests passed."
                                         else putStrLn "Some tests failed."
