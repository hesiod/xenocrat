module Main where

import GHC.Float
import Data.Monoid

import Constants
import Graphics.Gloss
import Render

main :: IO ()
main =
  let zoom = (3e6,1e1);
      bds = [earth,moon,sun];
      timeScale = 100000;
      stepsPerSecond = 10000;
      res = (1000, 1000);
      static = Color (makeColor 1 0 0 1) $ mconcat [Line [(-1,0),(1,0)], Line [(0,-1),(0,1)]]
      disp = InWindow "xenocrat" res (0, 0);
      render state = mconcat [picturizeState zoom state, static];
      update _ dt = updateState (timeScale * float2Double dt);
   in simulate disp white stepsPerSecond bds render update
