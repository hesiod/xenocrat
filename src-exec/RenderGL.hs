{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module RenderGL where

import Data.VectorSpace
import Data.Basis
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Control.Arrow

import Physics
import Common

type State v = [Body v]
type Zoom s = (s, s)
type Screen = (GLint, GLint)

scaleCoordinates :: (HasBasis v, AdditiveGroup v, Show s, Fractional s, s ~ Scalar v) => Screen -> s -> v -> (s, s)
scaleCoordinates scr zoom v = (x/scrX, y/scrY)
    where
      (scrX, scrY) = (fromIntegral *** fromIntegral) scr
      (x:y:_) = map snd . decompose . (^/zoom) $ v

black, blue, red, green :: Color4 GLfloat
black = Color4 0 0 0 1
blue = Color4 0 0 1 1
red = Color4 1 0 0 1
green = Color4 0 1 0 1

--rP pts = renderPrimitive Lines $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) pts

picturizeV :: (HasBasis v, AdditiveGroup v, Show s, Fractional s, MatrixComponent s, VertexComponent s, s ~ Scalar v) => Screen -> Zoom s -> Body v -> IO ()
picturizeV scr zoom b = do
  loadIdentity
  lineWidth $= 2
  translate $ Vector3 xR yR 0
  currentColor $= black
  renderObject Solid $ Sphere' 0.01 16 16
--  renderQuadric style $ Sphere 0.1 30 30
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
      fac = 1/0.07
      deg = 135 + 10
      deg' = 360 - 2*deg
--      style = QuadricStyle (Just Smooth) NoTextureCoordinates Outside LineStyle
      nullV = Vertex3 0 0 0 :: Vertex3 GLfloat
      (zoomR, zoomV) = zoom
      (xR, yR) = scaleCoordinates scr zoomR (pos b)
      (xV, yV) = scaleCoordinates scr zoomV (vel b)
      (xA, yA) = scaleCoordinates scr (zoomV*fac) (vel b)

picturizeState :: (HasBasis v, AdditiveGroup v, Show s, Fractional s, MatrixComponent s, VertexComponent s, s ~ Scalar v) => Screen -> Zoom s -> State v -> IO ()
picturizeState scr zoom = mapM_ (picturizeV scr zoom)

updateState :: forall v s. (InnerSpace v, Floating s, Eq s, Eq v, s ~ Scalar v) => s -> State v -> State v
updateState dt st = map updatePoint st
    where
      updatePoint :: Body v -> Body v
      updatePoint body = let st' = filter (/= body) st in dP body st' dt
