module Main where

import GHC.Float
import Data.Monoid

import Physics
import Graphics.Gloss
import Render

main :: IO ()
main = do
  let zoom = 3e6
  let bds = [earth,moon]
  let timeScale = 100000
  let stepsPerSecond = 10000 -- /100
  let static = Color (makeColor 1 0 0 1) $ mconcat [Line [(-1,0),(1,0)], Line [(0,-1),(0,1)]]
  let res = (1000, 1000);
      disp = InWindow "Hello" res (0, 0);
      render state = mconcat [static, picturizeState zoom state];
      update _ dt = updateState (timeScale * float2Double dt);
   in simulate disp white stepsPerSecond bds render update

{-print $ fG earth moon
  print . normalize . p $ moon
  print $ fA moon bds
  print $ vA moon bds dt
  print $ dP moon bds dt
-}

