{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module GL where

import Graphics.UI.GLUT
import Control.Arrow
import Data.IORef
import Data.VectorSpace
import Data.Metrology.Vector
import Data.Metrology.SI.Poly

import RenderGL
import Constants
import Common

glMain :: forall v s. (ConformingVector v, s ~ Scalar v, s ~ FT, v ~ (FT, FT)) => IO ()
glMain = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer, Multisampling, WithSamplesPerPixel 16]
  initialWindowSize $= Size 500 500
  _ <- createWindow progName
  bds <- newIORef [earth,moon,sun]
  screen <- newIORef (500,500)
  displayCallback $= displayState bds screen
  idleCallback $= Just (idle bds)
  reshapeCallback $= Just (reshape screen)
  mainLoop

reshape :: IORef (Screen GLfloat) -> Size -> IO ()
reshape screen s@(Size w h) = do
  viewport $= (Position 0 0, s)
  screen $= (fromIntegral w, fromIntegral h)

  matrixMode $= Projection
  loadIdentity
  let near = 1
      far = 5
      fov = 60
      ar = fromIntegral w / fromIntegral h
  perspective fov ar near far
  matrixMode $= Modelview 0

  print (w, h)

idle :: forall v s. ( ConformingVector v, s ~ Scalar v, s ~ FT) => IORef (State SI v) -> IO ()
idle state = do
  s <- get state
  state $= updateState (1000 % Second :: Time SI s) s
  postRedisplay Nothing

displayState :: IORef (State SI (Pair FT)) -> IORef (Screen GLfloat) -> IO ()
displayState state screen = do
  s <- get state
  scr <- get screen
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  hint LineSmooth $= Nicest
  hint PerspectiveCorrection $= Nicest
  clearColor $= white
  clear [ColorBuffer]

  loadIdentity
  lookAt (Vertex3 1 1 1) (Vertex3 0 0 0) (Vector3 0 1 0)
  rotate (90 :: GLfloat) $ Vector3 1 0 0

  color green
  displayCross

  let z = (100*3e6,1e2)
  let p = scaleCoordinates scr (fst z) . (negate *** negate) . (# Meter) . pos . head $ s
  --translate $ Vector3 (fst p) (snd p) 0

  color red
  displayCross
  picturizeState scr z s
  swapBuffers
