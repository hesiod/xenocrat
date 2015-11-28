{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Simulation where

import Control.Arrow
import Graphics.Rendering.OpenGL
import Data.Metrology.SI.Poly
import Data.Metrology.Poly
import Data.Metrology.Linear
import Data.Metrology.Unsafe
import Linear.V2
import Linear.Metric
import Linear.Affine

import Physics
import Common

--type TTriple s = Triple (Triple s)
--type Screen s = Pair s
--type Zoom s = Pair s
--class (Constraint a, Constraint (Scalar a)) => Both a :: a :: Constraint

--class (Real s, Eq s, Show s, Floating s, MatrixComponent s, VertexComponent s) => ConformingScalar s
--class (Eq v, HasBasis v, InnerSpace v, ConformingScalar (Scalar v)) => ConformingVector v

--instance ConformingScalar FT
--instance ConformingVector (Pair FT)

{-scaleCoordinates :: forall f a. (Functor f) => Screen sn -> Scalar v -> v -> Screen sn
scaleCoordinates scr zoom v = ((realToFrac a) / (realToFrac $ fst scr), (realToFrac b) / (realToFrac $ snd scr))
    where
      v' = v ^/ zoom
      l = take 2 . map snd . decompose $ v'
      (a:b:_) = l

scaleState :: forall f a. (Functor f) =>
              Screen (V2 a) -> Zoom a -> State SI f a -> [TTriple a]
scaleState scr zoom = map scaleBody
    where
      scaleBody :: Body SI f a -> TTriple a
      scaleBody ref = (veR, veV, veA)
          where
            fac = recip 0.3 :: a
            vV :: a -> f a -> Triple a
            vV z v = let p = scaleCoordinates scr z v :: Pair sn;
                     in (fst p, snd p, 0)
            veR = let zm = fst zoom
                  in vV zm (pos ref # Meter)
            veV = let zm = snd zoom
                  in vV zm (vel ref # (Meter :/ Second))
            veA = let zm = snd zoom * fac
                  in vV zm (vel ref # (Meter :/ Second))-}
