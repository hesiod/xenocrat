{-# LANGUAGE TypeFamilies, TypeOperators, ScopedTypeVariables, DataKinds, FlexibleContexts, ConstraintKinds #-}

module Physics where

import Data.VectorSpace
import Data.Metrology.Vector
import Data.Metrology.SI.Poly
--import Data.Metrology.SI.PolyTypes
import Common
import Constants

fG :: forall v s. (InnerSpace v, Floating s, s ~ Scalar v, s ~ FT) => Body SI v -> Body SI v -> Force SI v
fG a b = dir |^*| f
    where
      dp = pos a |-| pos b
      m = mass a |*| mass b
      mg = gamma |*| m
      r = qMagnitudeSq dp
      f = redim $ mg |/| r :: Force SI s
      dir = qNormalized dp
--      dir = qNormalized $ pos a |.-.| pos b :: Qu '[] SI v

fA :: (InnerSpace v, Floating s, s ~ Scalar v, s ~ FT) => Body SI v -> [Body SI v] -> Force SI v
fA ref bodies = qSum . map (fG ref) $ bodies

vA :: (InnerSpace v, Floating s, s ~ Scalar v, s ~ FT) => Body SI v -> [Body SI v] -> Time SI s -> Velocity SI v
vA ref bodies dt = vel ref |+| v
    where
      ffA = fA ref bodies
      v = (ffA |^*| dt) |^/| mass ref

dP :: (InnerSpace v, Floating s, s ~ Scalar v, s ~ FT) => Body SI v -> [Body SI v] -> Time SI s -> Body SI v
dP ref bodies dt = ref { pos = (vAvg |^*| dt) |+| pos ref }
    where
      v0 = vel ref
      v1 = vA ref bodies dt
      vAvg = (v0 |+| v1) |^/| 2
