module Physics where

import Vector
import Aux
import Debug.Trace
    
gamma :: FT
gamma = 6.6738e-11
ae :: FT
ae = 1.4960e11
moonPeriod = 2.360448e6
        
data Body = Body { mass :: FT, p :: Vec, v :: Vec } deriving (Eq, Show)

fG :: Body -> Body -> Vec
fG a b = dp ||* f
    where
      f = gamma * mass a * mass b / distanceSqrd (p a) (p b)
      dp = normalize $ p a |- p b

           
earth = Body 5.974e24 [0,0] [0,0] --29.7867]
moon = Body 7.349e22 [3.844e8,0] [0,1000]
--sun = Body 1.989e30 [-ae,0] [0,0]

fA :: Body -> [Body] -> Vec
--foldl :: (Vec -> Body -> Vec) -> Vec -> [Body] -> Vec
fA a l = foldl (\v b -> (fG b a) |+ v) (repeat 0) l

vA :: Body -> [Body] -> FT -> Vec
vA ref l dt = v1
    where
      ffA = fA ref l
      v0 = (ffA ||* dt) ||/ mass ref
      v1 = v0 |+ v ref

dP :: Body -> [Body] -> FT -> Body
dP ref l dt = Body (mass ref) ((vAvg ||* dt) |+ p ref) (v1)
    where
      v0 = v ref
      v1 = vA ref l dt
      vAvg = (v0 |+ v1) ||/ 2


             
{-
accelM :: Body -> Body -> Vec
accelM b = f ||/ mass b
    where
      f = fG a b |+ fZ b (p a - p b)
           
step :: Body -> Body
step -}

{-
fZ :: Body -> FT -> FT -> Vec
fZ a r v = dp ||* f
    where
      f = mass a * v^2 / r
      dp = normalize . p $ a
-}
       
