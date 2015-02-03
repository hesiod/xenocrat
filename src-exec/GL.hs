module GL where

import Graphics.UI.GLUT
import Data.IORef

import RenderGL
import Constants

glMain :: IO ()
glMain = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer, Multisampling, WithSamplesPerPixel 4]
  initialWindowSize $= Size 500 500
  _ <- createWindow progName
  bds <- newIORef [earth,moon]
  screen <- newIORef (500,500)
  displayCallback $= displayState bds screen
  idleCallback $= Just (idle bds)
  reshapeCallback $= Just (reshape screen)
  mainLoop

reshape :: IORef (Screen GLfloat) -> Size -> IO ()
reshape screen s@(Size w h) = do
  viewport $= (Position 0 0, s)
  screen $= (fromIntegral w, fromIntegral h)
  print (w, h)

idle :: IORef (State (GLfloat, GLfloat)) -> IO ()
idle state = do
  s <- get state
  state $= updateState 1000 s
  postRedisplay Nothing

displayState :: IORef (State (Pair GLfloat)) -> IORef (Screen GLfloat) -> IO ()
displayState state screen = do
  s <- get state
  scr <- get screen

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  clearColor $= Color4 1 1 1 0
  clear [ColorBuffer]
  loadIdentity

  matrixMode $= Projection
  loadIdentity
  let near = 0
      far = 5
      right = 1
      top = 1
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0

  displayCross
  picturizeState scr (3e6,1e1) s
  swapBuffers

displayCross :: IO ()
displayCross = do
  loadIdentity
  color red
  lineWidth $= 0.5
  renderPrimitive Lines $ do
    vertex (Vertex3 (-1) 0 0 :: Vertex3 GLfloat)
    vertex (Vertex3 1 0 0    :: Vertex3 GLfloat)
  renderPrimitive Lines $ do
    vertex (Vertex3 0 (-1) 0 :: Vertex3 GLfloat)
    vertex (Vertex3 0 1 0    :: Vertex3 GLfloat)
  renderPrimitive Lines $ do
    vertex (Vertex3 0 0 (-1) :: Vertex3 GLfloat)
    vertex (Vertex3 0 0 1    :: Vertex3 GLfloat)
