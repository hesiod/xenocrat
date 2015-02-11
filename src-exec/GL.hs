module GL where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Control.Concurrent
import System.Exit

import RenderGL
import Constants

glMain :: IO ()
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
  keyboardMouseCallback $= Just keyboard

  mainLoop

keyboard :: Key -> KeyState -> a -> b -> IO ()
keyboard (Char '\27') Down _ _ = exitSuccess
keyboard _ _ _ _ = return ()

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

idle :: IORef (State (Pair FT)) -> IO ()
idle state = do
  s <- get state
  state $= updateState 10 s
  postRedisplay Nothing

displayState :: IORef (State (Pair FT)) -> IORef (Screen GLfloat) -> IO ()
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

  color red
  displayCross
  picturizeState scr (0.3*3e6,1e2) s
  swapBuffers