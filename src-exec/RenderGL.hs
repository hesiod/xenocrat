{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module RenderGL where

import Data.VectorSpace
import Data.VectorSpace.OpenGL()
import Data.Basis
import Graphics.UI.GLUT
import Control.Arrow

import Physics
import Common

type State v = [Body v]
type Pair s = (s, s)
type Screen s = Pair s
type Zoom s = Pair s

class (Real s, Eq s, Show s, Floating s, MatrixComponent s, VertexComponent s) => ConformingScalar s where
class (Eq v, HasBasis v, InnerSpace v, ConformingScalar (Scalar v)) => ConformingVector v where

instance ConformingScalar GLfloat where
instance ConformingVector (Pair GLdouble) where
instance ConformingScalar GLdouble where

convertToDisplay :: (ConformingScalar s, ConformingScalar sn) => (s, s) -> (sn, sn)
convertToDisplay = realToFrac *** realToFrac
convertToWorld :: (ConformingScalar s, ConformingScalar sn) => (sn, sn) -> (s, s)
convertToWorld = realToFrac *** realToFrac

scaleCoordinates :: (ConformingVector v, s ~ Scalar v, ConformingScalar sn) => Screen sn -> s -> v -> (sn, sn)
scaleCoordinates scr zoom v = (c/x, d/y)
    where
      (x, y) = convertToWorld scr
      (a:b:_) = map snd . decompose . (^/zoom) $ v
      (c, d) = convertToDisplay (a, b)

black, white, blue, red, green :: Color4 GLfloat
black = Color4 0 0 0 1
white = Color4 1 1 1 1
blue = Color4 0 0 1 1
red = Color4 1 0 0 1
green = Color4 0 1 0 1

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
    renderObject Solid $ Sphere' 0.01 16 16
  where
      toVe (a, b, c) = Vector3 a b c
      toVx (a, b, c) = Vertex3 a b c
      deg = 135 + 10 :: sn
      zAxis = Vector3 0 0 (1::sn)
      line :: Vertex3 sn -> IO ()
      line v = renderPrimitive Lines $ do
                 vertex $ Vertex3 0 0 (0::GLfloat)
                 vertex v
      lineA deg = preservingMatrix $ do
        rotate deg zAxis
        line $ toVx veA

scaleState :: forall v s sn vx vex3. (ConformingVector v, s ~ Scalar v, ConformingScalar sn, vx ~ (sn, sn, sn), vex3 ~ (vx, vx, vx)) =>
              Screen sn -> Zoom s -> State v -> [vex3]
scaleState scr zoom = map scaleBody
    where
      (zoomR, zoomV) = zoom
      scaleBody :: Body v -> vex3
      scaleBody ref = (veR, veV, veA)
          where
            fac = 1/0.3 :: s
            vV z v = let p = convertToDisplay $ scaleCoordinates scr z v :: Pair sn; in (fst p, snd p, 0)
            veR = vV zoomR (pos ref)
            veV = vV zoomV (vel ref)
            veA = vV (zoomV*fac) (vel ref)

picturizeState :: (ConformingVector v, s ~ Scalar v, ConformingScalar sn) => Screen sn -> Zoom s -> State v -> IO ()
picturizeState scr zoom = mapM_ picturizeV . scaleState scr zoom

updateState :: forall v s. (ConformingVector v, s ~ Scalar v) => s -> State v -> State v
updateState dt st = map updatePoint st
    where
      updatePoint :: Body v -> Body v
      updatePoint body = let st' = filter (/= body) st in dP body st' dt

displayCross :: IO ()
displayCross =
  preservingMatrix $ do
    lineWidth $= 0.8
    let l = 1000
    renderPrimitive Lines $ do
      vertex (Vertex3 (-l) 0 0 :: Vertex3 GLfloat)
      vertex (Vertex3 l 0 0    :: Vertex3 GLfloat)
    renderPrimitive Lines $ do
      vertex (Vertex3 0 (-l) 0 :: Vertex3 GLfloat)
      vertex (Vertex3 0 l 0    :: Vertex3 GLfloat)
    renderPrimitive Lines $ do
      vertex (Vertex3 0 0 (-l) :: Vertex3 GLfloat)
      vertex (Vertex3 0 0 l    :: Vertex3 GLfloat)
