{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module RenderGL where

import Data.VectorSpace
import Data.Basis
import Graphics.UI.GLUT
import Control.Arrow

import Physics
import Common

type State v = [Body v]
type Pair s = (s, s)
type Screen s = Pair s
type Zoom s = Pair s

scaleCoordinates :: (HasBasis v, AdditiveGroup v, Show s, Fractional s, s ~ Scalar v) => Screen s -> s -> v -> (s, s)
scaleCoordinates scr zoom v = (a/) *** (b/) $ scr
    where
      (a:b:_) = map snd . decompose . (^/zoom) $ v

black, white, blue, red, green :: Color4 GLfloat
black = Color4 0 0 0 1
white = Color4 1 1 1 1
blue = Color4 0 0 1 1
red = Color4 1 0 0 1
green = Color4 0 1 0 1

picturizeV :: (HasBasis v, AdditiveGroup v, Show s, Fractional s, MatrixComponent s, VertexComponent s, s ~ Scalar v) => Screen s -> Zoom s -> Body v -> IO ()
picturizeV scr zoom b =
    preservingMatrix $ do
      lineWidth $= 2
      translate $ Vector3 xR yR 0
      currentColor $= black
      renderObject Solid $ Sphere' 0.01 16 16
      currentColor $= blue
      renderPrimitive Lines $ do
                         vertex nullV
                         vertex $ Vertex3 xV yV 0
      translate $ Vector3 xV yV 0
      rotate deg $ Vector3 0 0 (1::GLfloat)
      renderPrimitive Lines $ do
                         vertex nullV
                         vertex $ Vertex3 xA yA 0
      rotate deg' $ Vector3 0 0 (1::GLfloat)
      renderPrimitive Lines $ do
                         vertex nullV
                         vertex $ Vertex3 xA yA 0
  where
      fac = 1/0.3
      deg = 135 + 10
      deg' = 360 - 2*deg
--    style = QuadricStyle (Just Smooth) NoTextureCoordinates Outside LineStyle
--    renderQuadric style $ Sphere 0.1 30 30
      nullV = Vertex3 0 0 0 :: Vertex3 GLfloat
      (zoomR, zoomV) = zoom
      (xR, yR) = scaleCoordinates scr zoomR (pos b)
      (xV, yV) = scaleCoordinates scr zoomV (vel b)
      (xA, yA) = scaleCoordinates scr (zoomV*fac) (vel b)

picturizeState :: (HasBasis v, AdditiveGroup v, Show s, Fractional s, MatrixComponent s, VertexComponent s, s ~ Scalar v) => Screen s -> Zoom s -> State v -> IO ()
picturizeState scr zoom = mapM_ (picturizeV scr zoom)

updateState :: forall v s. (InnerSpace v, Floating s, Eq s, Eq v, s ~ Scalar v) => s -> State v -> State v
updateState dt st = map updatePoint st
    where
      updatePoint :: Body v -> Body v
      updatePoint body = let st' = filter (/= body) st in dP body st' dt
