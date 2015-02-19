{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module GL where

import Control.DeepSeq
import Control.Monad
import Control.Concurrent
import Control.Applicative
import Data.IORef
import Data.VectorSpace
import Data.Metrology.Vector
import Data.Metrology.Show()
import Data.Metrology.SI.Poly
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Linear
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit

import Constants
import Common

import RenderGL
import Simulation
import GLHelper
import Shaders

type MVAO = Map String VAO
type MSP = Map String ShaderProgram
type MB = Map String BufferObject

errorC :: G.ErrorCallback
errorC _ = putStrLn

dollySpeed :: DT
dollySpeed = 0.1
rotateSpeed :: DT
rotateSpeed = 1
keyboardC :: IORef (Camera DT) -> G.KeyCallback
keyboardC _ window G.Key'Escape _ G.KeyState'Pressed _ = G.setWindowShouldClose window True
keyboardC cam _ G.Key'R _ G.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 0 dollySpeed 0)
keyboardC cam _ G.Key'F _ G.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 0 (-dollySpeed) 0)
keyboardC cam _ G.Key'A _ G.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 (-dollySpeed) 0 0)
keyboardC cam _ G.Key'D _ G.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 dollySpeed 0 0)
keyboardC cam _ G.Key'W _ G.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 0 0 (-dollySpeed))
keyboardC cam _ G.Key'S _ G.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 0 0 dollySpeed)
keyboardC cam _ G.Key'Q _ G.KeyState'Repeating _ = modifyIORef cam $ roll (-rotateSpeed)
keyboardC cam _ G.Key'E _ G.KeyState'Repeating _ = modifyIORef cam $ roll rotateSpeed
keyboardC cam _ G.Key'Up _ G.KeyState'Repeating _ = modifyIORef cam $ tilt rotateSpeed
keyboardC cam _ G.Key'Down _ G.KeyState'Repeating _ = modifyIORef cam $ tilt (-rotateSpeed)
keyboardC cam _ G.Key'Left _ G.KeyState'Repeating _ = modifyIORef cam $ pan (-rotateSpeed)
keyboardC cam _ G.Key'Right _ G.KeyState'Repeating _ = modifyIORef cam $ pan rotateSpeed
keyboardC _ _ _ _ _ _ = return ()

reshapeC :: IORef (Screen DT) -> G.WindowSizeCallback
reshapeC screen _ w h = do
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  screen $= (fromIntegral w, fromIntegral h)
  print (w, h)

windowHints :: IO ()
windowHints =
  mapM_ G.windowHint [ G.WindowHint'Samples 16,
                       G.WindowHint'Decorated False,
                       G.WindowHint'OpenGLProfile G.OpenGLProfile'Core,
                       G.WindowHint'ContextVersionMajor 4,
                       G.WindowHint'ContextVersionMinor 4]

init :: (G.Window -> IO ()) -> IO ()
init main = do
  G.setErrorCallback (Just errorC)
  successfulInit <- G.init
  if successfulInit then do
    windowHints
    mw <- G.createWindow 640 480 "xenocrat" Nothing Nothing
    case mw of Nothing -> do
                G.terminate
                exitFailure
               Just window -> do
                G.makeContextCurrent mw
                main window
                G.destroyWindow window
                G.terminate
                exitSuccess
  else exitFailure

glMain :: forall v s. (ConformingVector v, s ~ Scalar v, s ~ FT, v ~ (FT, FT)) => IO ()
glMain = GL.init (\window -> do
    let prepare p = bindFragDataLocation p "outColor" $= 0
        setup sp = makeVAO $ do
               enableAttrib sp "position"
               setAttrib sp "position" ToFloat $ VertexArrayDescriptor 3 Float 0 offset0
    crossSP <- buildShader [passthroughVS, crossGS, defaultFS] prepare
    vectorSP <- buildShader [defaultVS, vectorGS, defaultFS] prepare
    crossB <- makeBuffer ArrayBuffer [V3 0 0 0 :: V3 DT]
    crossV <- setup crossSP
    vectorB <- makeBuffer ArrayBuffer (replicate 6 $ V3 0 0 0 :: [V3 DT])
    vectorV <- setup vectorSP
    let bufs = Map.fromList [("cross", crossB), ("vector", vectorB)] :: MB
        sps = Map.fromList [("cross", crossSP), ("vector", vectorSP)] :: MSP
        vaos = Map.fromList [("cross", crossV), ("vector", vectorV)] :: MVAO

    bds <- newIORef [earth, moon, sun]
    screen <- newIORef (1024, 768)
    cam <- newIORef (fpsCamera :: Camera DT)
    let display = displayState bufs sps vaos cam bds screen
        reshape = reshapeC screen
    G.swapInterval 1
    G.setKeyCallback window $ Just $ keyboardC cam
    G.setWindowRefreshCallback window $ Just display
    G.setWindowSizeCallback window $ Just reshape

    _ <- forkIO $ forever $ do
           s <- readIORef bds
           let s' = updateState (10 % Second :: Time SI s) s
           s' `deepseq` writeIORef bds s'
    display window
    )

displayState :: MB -> MSP -> MVAO -> IORef (Camera DT) -> IORef (State SI (Pair FT)) -> IORef (Screen GLfloat) -> G.WindowRefreshCallback
displayState bufs sps vaos cam bds screen w = forever $ do
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  clearColor $= blue
  clear [ColorBuffer, DepthBuffer, StencilBuffer]

  camera <- get cam
  scr <- get screen

  let cS = sps ! "cross"
  let cV = vaos ! "cross"
  let cB = bufs ! "cross"
  let vS = sps ! "vector"
  let vV = vaos ! "vector"
  let vB = bufs ! "vector"

  let rot = m33_to_m44 $ fromQuaternion $ axisAngle (V3 1 0 0) (0 * pi / 180) :: M44 DT
      arrowrot = m33_to_m44 $ fromQuaternion $ axisAngle (V3 0 0 1) (60 * pi / 180) :: M44 DT -- (V3 0 1 0)
      look = Linear.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)
      view = look !*! camMatrix camera -- rot
      near = 0.01
      far = 10
      fov = 0.8
      ar = uncurry (/) scr
      proj = Linear.perspective fov ar near far
      arrowscale = 0.6 :: DT
      zoom = 3e11 :: DT

  lineWidth $= 0.1

  currentProgram $= Just (program cS)
  setUniform cS "view" view
  setUniform cS "proj" proj
  setUniform cS "model" (eye4 :: M44 DT)
  withVAO cV $ do
    bindBuffer ArrayBuffer $= Just cB
    drawArrays Points 0 1

  s <- get bds
  currentProgram $= Just (program vS)
  setUniform vS "view" view
  setUniform vS "proj" proj
  setUniform vS "model" (eye4 :: M44 DT)
  setUniform vS "arrowrot" arrowrot
  setUniform vS "arrowscale" arrowscale
  setUniform vS "zoom" zoom
  let verts = fmap realToFrac <$> concatMap bodyVertices s :: [V3 DT]
  withVAO vV $ do
    bindBuffer ArrayBuffer $= Just vB
    replaceBuffer ArrayBuffer verts
    drawArrays Lines 0 (fromIntegral $ length verts)

  throwError -- get errors >>= (\e -> unless (null e) $ print e)

  G.swapBuffers w
  G.pollEvents
