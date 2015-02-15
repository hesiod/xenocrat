{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module GL where

import Control.DeepSeq
import Control.Monad
import Control.Concurrent
import Data.IORef
import System.Exit

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import Data.VectorSpace
import Data.Metrology.Vector
import Data.Metrology.SI.Poly

import RenderGL
import Constants
import Common

errorC :: G.ErrorCallback
errorC _ = putStrLn

keyboardC :: G.KeyCallback
keyboardC window key _ action _ = when (key == G.Key'Escape && action == G.KeyState'Pressed) $
  G.setWindowShouldClose window True

glMain :: forall v s. (ConformingVector v, s ~ Scalar v, s ~ FT, v ~ (FT, FT)) => IO ()
glMain = do
  G.setErrorCallback (Just errorC)
  successfulInit <- G.init
  if successfulInit then do
                      bds <- newIORef [earth,moon,sun]
                      screen <- newIORef (500,500)
                      G.windowHint $ G.WindowHint'Samples 16
                      G.windowHint $ G.WindowHint'Decorated False
--                    G.windowHint $ G.WindowHint'OpenGLProfile G.OpenGLProfile'Compat
--                    G.windowHint $ G.WindowHint'ContextVersionMajor 3
--                    G.windowHint $ G.WindowHint'ContextVersionMinor 2
                      mw <- G.createWindow 640 480 "xenocrat" Nothing Nothing
                      case mw of Nothing -> do
                                   G.pollEvents
                                   G.terminate
                                   exitFailure
                                 Just window -> do
                                   let display = displayState bds screen
                                   G.makeContextCurrent mw
                                   G.swapInterval 1
                                   G.setKeyCallback window (Just keyboardC)
                                   G.setWindowRefreshCallback window (Just display)
                                   G.setWindowSizeCallback window (Just (reshape screen))
                                   _ <- forkIO $ forever $ do
                                                  s <- readIORef bds
                                                  let s' = updateState (100 % Second :: Time SI s) s
                                                  s' `deepseq` writeIORef bds s'
                                   display window
                                   G.destroyWindow window
                                   G.terminate
                                   exitSuccess
  else exitFailure

reshape :: IORef (Screen GLfloat) -> G.Window -> Int -> Int -> IO ()
reshape screen _ w h = do
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
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

displayState :: IORef (State SI (Pair FT)) -> IORef (Screen GLfloat) -> G.Window -> IO ()
displayState bds screen w = forever $ do
  scr <- get screen
  s <- get bds

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  clearColor $= white
  clear [ColorBuffer]

  loadIdentity
  lookAt (Vertex3 1 1 1) (Vertex3 0 0 0) (Vector3 0 1 0)
  rotate (90 :: GLfloat) $ Vector3 1 0 0

  color green
  displayCross
  let z = (100*3e6, 1e2)
  --let p = scaleCoordinates scr (fst z) . (negate *** negate) . (# Meter) . pos . head $ s
  --translate $ Vector3 (fst p) (snd p) 0

  color red
  displayCross
  picturizeState scr z s

  G.swapBuffers w
  G.pollEvents
