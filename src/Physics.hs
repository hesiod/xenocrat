module Physics where

import Vector
import Common
import Constants

fG :: Body -> Body -> Vec
fG a b = dp ||* f
    where
      f = gamma * mass a * mass b / distanceSqrd (pos a) (pos b)
      dp = normalize $ pos a |- pos b

fA :: Body -> [Body] -> Vec
--foldl :: (Vec -> Body -> Vec) -> Vec -> [Body] -> Vec
fA a = foldl (\v b -> fG b a |+ v) (repeat 0)

vA :: Body -> [Body] -> Double -> Vec
vA ref l dt = v1
    where
      ffA = fA ref l
      v0 = (ffA ||* dt) ||/ mass ref
      v1 = v0 |+ vel ref

dP :: Body -> [Body] -> Double -> Body
dP ref l dt = Body (mass ref) ((vAvg ||* dt) |+ pos ref) v1
    where
      v0 = vel ref
      v1 = vA ref l dt
      vAvg = (v0 |+ v1) ||/ 2

{-
accelM :: Body -> Body -> Vec
accelM b = f ||/ mass b
    where
      f = fG a b |+ fZ b (pos a - pos b)

fZ :: Body -> Double -> Double -> Vec
fZ a r v = dpos ||* f
    where
      f = mass a * v^2 / r
      dpos = normalize . pos $ a
-}
