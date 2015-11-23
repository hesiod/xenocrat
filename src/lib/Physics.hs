{-# LANGUAGE TypeFamilies, TypeOperators, ScopedTypeVariables, DataKinds, FlexibleContexts, ConstraintKinds #-}

module Physics where

import Data.VectorSpace
import Data.Metrology.Vector
import Data.Metrology.SI.Poly
--import Data.Metrology.SI.PolyTypes
import Common
import Constants

fG :: forall v. (InnerSpace v, Floating (Scalar v)) => Body SI v -> Body SI v -> Force SI v
fG a b = dir |^*| f
    where
      dp = pos b |-| pos a
      m = gamma |*| mass a |*| mass b
      r = qMagnitudeSq dp
      f = redim $ m |/| r :: Force SI (Scalar v)
      dir = qNormalized dp
--      dir = qNormalized $ pos a |.-.| pos b :: Qu '[] SI v

fA :: (InnerSpace v, Floating (Scalar v)) => Body SI v -> [Body SI v] -> Force SI v
fA ref bodies = qSum . map (fG ref) $ bodies

vA :: (InnerSpace v, Floating (Scalar v)) => Body SI v -> [Body SI v] -> Time SI (Scalar v) -> Velocity SI v
vA ref bodies dt = vel ref |+| v
    where
      ffA = fA ref bodies
      v = (ffA |^*| dt) |^/| mass ref

dP :: (InnerSpace v, Floating (Scalar v)) => Body SI v -> [Body SI v] -> Time SI (Scalar v) -> Body SI v
dP ref bodies dt = ref { vel = v1, pos = (vAvg |^*| dt) |+| pos ref }
    where
      v0 = vel ref
      v1 = vA ref bodies dt
      vAvg = (v0 |+| v1) |^/| 2
