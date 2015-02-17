{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module RenderGL where

import Data.VectorSpace
import Data.VectorSpace.OpenGL()
import Data.Metrology.SI.Poly
import Graphics.Rendering.OpenGL

import Common
import Simulation
import GLHelper

picturizeV :: forall sn vx vex3. (ConformingScalar sn, vx ~ (sn, sn, sn), vex3 ~ (vx, vx, vx)) => vex3 -> IO ()
picturizeV (veR, veV, veA) = do
  lineWidth $= 2
  preservingMatrix $ do
    translate $ toVe veR
    color blue
    line $ toVx veV
    preservingMatrix $ do
                       translate $ toVe veV
                       lineA deg
                       lineA (-deg)
    color black
    -- render sphere here
  where
      toVe (a, b, c) = Vector3 a b c
      toVx (a, b, c) = Vertex3 a b c
      deg = 135 + 10 :: sn
      zAxis = Vector3 0 0 (1::sn)
      line :: Vertex3 sn -> IO ()
      line v = renderPrimitive Lines $ do
                               vertex $ Vertex3 0 0 (0::GLfloat)
                               vertex v
      lineA d = preservingMatrix $ do
                               rotate d zAxis
                               line $ toVx veA

picturizeState :: (ConformingVector v, s ~ Scalar v, s ~ FT, ConformingScalar sn) => Screen sn -> Zoom s -> State SI v -> IO ()
picturizeState scr zoom = mapM_ picturizeV . scaleState scr zoom
