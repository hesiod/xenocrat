module Main where

import Physics
import Vector
import Graphics.Gloss
import Render
  
main :: IO ()
main = do
  print $ fG earth moon
  print . normalize . p $ moon
  let dt = 100
  let zoom = 3e7
  let bds = [earth]
  let abds = moon:bds
  print $ fA moon bds
  print $ vA moon bds dt
  print $ dP moon bds dt
  let res = (500, 500)
  let disp = InWindow "Hello" res (0, 0)
  let sts = states dt zoom abds
  animate disp white (\sec -> sts !! (36 * floor sec))

