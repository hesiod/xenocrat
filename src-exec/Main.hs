{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Monoid
import Graphics.Gloss

import Constants
import Render

main :: IO ()
main =
  let zoom = (3e6,1e1);
      bds = [earth,moon];
      timeScale = 100000;
      stepsPerSecond = 10000;
      res = (1000, 1000);
      static = Color (makeColor 1 0 0 1) $ mconcat [Line [(-1,0),(1,0)], Line [(0,-1),(0,1)]]
      disp = InWindow "xenocrat" res (0, 0);
      render state = mconcat [picturizeState zoom state, static];
      update _ dt = updateState (timeScale * dt);
   in simulate disp white stepsPerSecond bds render update
