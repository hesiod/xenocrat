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
type Screen s = Pair s
type Zoom s = Pair s

class (Real s, Eq s, Show s, Floating s, MatrixComponent s, VertexComponent s) => ConformingScalar s where
class (Eq v, HasBasis v, InnerSpace v, ConformingScalar (Scalar v)) => ConformingVector v where

instance ConformingScalar GLfloat
instance ConformingVector (Pair GLdouble)
instance ConformingScalar GLdouble

scaleCoordinates :: (ConformingVector v, s ~ Scalar v, ConformingScalar sn) => Screen sn -> s -> v -> (sn, sn)
scaleCoordinates scr zoom v = (c/x, d/y)
    where
      cv :: (ConformingScalar s, ConformingScalar sn) => (s, s) -> (sn, sn)
      cv = realToFrac *** realToFrac
      (x, y) = cv scr
      (a:b:_) = map snd . decompose . (^/zoom) $ v
      (c, d) = cv (a, b)

scaleState :: forall v s sn vx vex3. (ConformingVector v, s ~ Scalar v, s ~ FT, ConformingScalar sn, vx ~ (sn, sn, sn), vex3 ~ (vx, vx, vx)) =>
              Screen sn -> Zoom s -> State SI v -> [vex3]
scaleState scr zoom = map scaleBody
    where
      (zoomR, zoomV) = zoom
      scaleBody :: Body SI v -> vex3
      scaleBody ref = (veR, veV, veA)
          where
            fac = 1/0.3 :: s
            vV :: (ConformingVector vv, s ~ Scalar vv, s ~ FT) => s -> vv -> (sn, sn, sn)
            vV z v = let p = scaleCoordinates scr z v :: Pair sn;
                     in (fst p, snd p, 0)
            veR = vV zoomR (pos ref # Meter)
            veV = vV zoomV (vel ref # (Meter :/ Second))
            veA = vV (zoomV*fac) (vel ref # (Meter :/ Second))

updateState :: forall v s. (NFData v, ConformingVector v, s ~ Scalar v, s ~ FT) => Time SI s -> State SI v -> State SI v
updateState dt st = parMap rdeepseq (\b -> let st' = filter (/= b) st in dP b st' dt) st
