{-# LANGUAGE TypeFamilies #-}

module Physics where

import Data.VectorSpace
import Common

fG :: (InnerSpace v, Floating s, s ~ Scalar v) => Body v -> Body v -> v
fG a b = ndp ^* f
    where
      dp = pos b ^-^ pos a
      gamma = 6.6738e-11
      f = gamma * mass a * mass b / magnitudeSq dp
      ndp = normalized dp

fA :: (InnerSpace v, Floating s, s ~ Scalar v) => Body v -> [Body v] -> v
fA ref bodies = sumV . map (fG ref) $ bodies

vA :: (InnerSpace v, Floating s, s ~ Scalar v) => Body v -> [Body v] -> s -> v
vA ref bodies dt = vel ref ^+^ v
    where
      ffA = fA ref bodies
      v = (ffA ^* dt) ^/ mass ref

dP :: (InnerSpace v, Floating s, s ~ Scalar v) => Body v -> [Body v] -> s -> Body v
dP ref bodies dt = Body (mass ref) ((vAvg ^* dt) ^+^ pos ref) v1
    where
      v0 = vel ref
      v1 = vA ref bodies dt
      vAvg = (v0 ^+^ v1) ^/ 2

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
