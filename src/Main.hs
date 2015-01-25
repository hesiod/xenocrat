module Main where

import Physics
import Vector
import Graphics.Gloss
import Render
  
main :: IO ()
main = do
  print $ fG earth moon
  print . normalize . p $ moon
  let dt = 1
  let zoom = 3e6
  let bds = [earth]
  let abds = moon:bds
  print $ fA moon bds
  print $ vA moon bds dt
  print $ dP moon bds dt
  let res = (500, 500)
  let disp = InWindow "Hello" res (0, 0)
  let sts = states dt 1 abds
  simulate disp white 0 0 (\sec -> sts !! floor sec)

