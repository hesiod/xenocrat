module Physics where

import Vector
import Common

gamma :: FT
gamma = 6.6738e-11
ae :: FT
ae = 1.4960e11
moonPeriod :: FT
moonPeriod = 2.360448e6

data Body = Body { mass :: FT, pos :: Vec, vel :: Vec } deriving (Eq, Show)

fG :: Body -> Body -> Vec
fG a b = dp ||* f
    where
      f = gamma * mass a * mass b / distanceSqrd (pos a) (pos b)
      dp = normalize $ pos a |- pos b

earth :: Body
earth = Body 5.974e24 [0,0] [0,0] --29.7867]
moon :: Body
moon = Body 7.349e22 [3.844e8,0] [0,1000]
sun :: Body
sun = Body 1.989e30 [-ae,0] [0,0]

fA :: Body -> [Body] -> Vec
--foldl :: (VELec -> Body -> VELec) -> VELec -> [Body] -> VELec
fA a = foldl (\v b -> fG b a |+ v) (repeat 0)

vA :: Body -> [Body] -> FT -> Vec
vA ref l dt = v1
    where
      ffA = fA ref l
      v0 = (ffA ||* dt) ||/ mass ref
      v1 = v0 |+ vel ref

dP :: Body -> [Body] -> FT -> Body
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

fZ :: Body -> FT -> FT -> Vec
fZ a r v = dpos ||* f
    where
      f = mass a * v^2 / r
      dpos = normalize . pos $ a
-}

