{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck.All
import Control.Monad

main :: IO ()
main = void $ $quickCheckAll
