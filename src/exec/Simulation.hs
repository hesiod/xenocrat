{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Simulation where

import Control.Arrow
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL
import Data.VectorSpace
import Data.VectorSpace.OpenGL()
import Data.Metrology.Vector
import Data.Metrology.SI.Poly
import Data.Basis

import Physics
import Common

type State l v = [Body l v]
type Pair s = (s, s)
type Triple s = (s, s, s)
type TTriple s = Triple (Triple s)
type Screen s = Pair s
type Zoom s = Pair s
--class (Constraint a, Constraint (Scalar a)) => Both a :: a :: Constraint

class (Real s, Eq s, Show s, Floating s, MatrixComponent s, VertexComponent s) => ConformingScalar s
class (Eq v, HasBasis v, InnerSpace v, ConformingScalar (Scalar v)) => ConformingVector v

--instance ConformingScalar FT
--instance ConformingVector (Pair FT)

scaleCoordinates :: forall s v sn. (Real s, ConformingVector v, ConformingScalar (Scalar v), ConformingScalar sn) => Screen sn -> Scalar v -> v -> Screen sn
scaleCoordinates scr zoom v = ((realToFrac a) / (realToFrac $ fst scr), (realToFrac b) / (realToFrac $ snd scr))
    where
      v' = v ^/ zoom
      l = take 2 . map snd . decompose $ v'
      (a:b:_) = l

scaleState :: forall v sn. (ConformingVector v, ConformingScalar sn) =>
              Screen sn -> Zoom (Scalar v) -> State SI v -> [TTriple sn]
scaleState scr zoom = map scaleBody
    where
      scaleBody :: Body SI v -> TTriple sn
      scaleBody ref = (veR, veV, veA)
          where
            fac = recip 0.3 :: Scalar v
            vV :: Scalar v -> v -> Triple sn
            vV z v = let p = scaleCoordinates scr z v :: Pair sn;
                     in (fst p, snd p, 0)
            veR = let zm = fst zoom
                  in vV zm (pos ref # Meter)
            veV = let zm = snd zoom
                  in vV zm (vel ref # (Meter :/ Second))
            veA = let zm = snd zoom * fac
                  in vV zm (vel ref # (Meter :/ Second))

updateState :: (NFData v, NFData (Scalar v), ConformingVector v) => Time SI (Scalar v) -> State SI v -> State SI v
updateState dt st = parMap rdeepseq (\b -> let st' = filter (/= b) st in dP b st' dt) st
