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

type SBV = (ShaderProgram, BufferObject, VAO)
type MSBV = Map String SBV

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
    planetSP <- buildShader [planetVS, planetTCS, planetTES, defaultFS] prepare
    crossB <- makeBuffer ArrayBuffer [V3 0 0 0 :: V3 DT]
    crossV <- setup crossSP
    planetB <- makeBuffer ArrayBuffer [V3 0 0 0 :: V3 DT]
    planetV <- setup planetSP
    vectorB <- makeBuffer ArrayBuffer (replicate 6 $ V3 0 0 0 :: [V3 DT])
    vectorV <- setup vectorSP
    let sbv = Map.fromList [("cross", (crossSP, crossB, crossV)), ("vector", (vectorSP, vectorB, vectorV)), ("planet", (planetSP, planetB, planetV))] :: MSBV

    bds <- newIORef [earth, moon, sun]
    screen <- newIORef (1024, 768)
    cam <- newIORef (fpsCamera :: Camera DT)
    let display = displayState sbv cam bds screen
        reshape = reshapeC screen
    G.swapInterval 1
    G.setKeyCallback window $ Just $ keyboardC cam
    G.setWindowRefreshCallback window $ Just display
    G.setWindowSizeCallback window $ Just reshape

    _ <- forkIO $ forever $ do
           s <- readIORef bds
           let s' = updateState (10 % Second :: Time SI s) s
           s' `deepseq` writeIORef bds s'
    patchVertices $= 3
    lineWidth $= 0.1
    display window
    )


displayState :: MSBV -> IORef (Camera DT) -> IORef (State SI (Pair FT)) -> IORef (Screen GLfloat) -> G.WindowRefreshCallback
displayState sbv cam bds screen w = forever $ do
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  clearColor $= blue
  clear [ColorBuffer, DepthBuffer, StencilBuffer]

  camera <- get cam
  scr <- get screen

  let (cS, cB, cV) = sbv ! "cross"
  let (pS, pB, pV) = sbv ! "planet"
  let (vS, vB, vV) = sbv ! "vector"

  let mkRot axis rad = m33_to_m44 . fromQuaternion $ axisAngle axis rad
      arrowrot = mkRot (V3 0 0 1) (deg2rad 60) :: M44 DT
      look = Linear.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)
      view = look !*! camMatrix camera
      near = 0.01
      far = 10
      fov = 0.8
      ar = uncurry (/) scr
      proj = Linear.perspective fov ar near far
      arrowscale = 0.6 :: DT
      zoom = 3e11 :: DT
      zoomP = 10 :: DT
      eye = eye4 :: M44 DT
      tessI = 10 :: DT
      tessO = 10 :: DT

  currentProgram $= Just (program cS)
  setUniform cS "view" view
  setUniform cS "proj" proj
  setUniform cS "model" eye
  bindBuffer ArrayBuffer $= Just cB
  withVAO cV $ drawArrays Points 0 1

  currentProgram $= Just (program pS)
  setUniform pS "view" view
  setUniform pS "proj" proj
  setUniform pS "model" eye
  setUniform pS "zoom" zoomP
  setUniform pS "tess_inner" tessI
  setUniform pS "tess_outer" tessO
  let vertsI = fmap realToFrac <$> icosahedronTriangles :: [V3 DT]
  bindBuffer ArrayBuffer $= Just pB
  replaceBuffer ArrayBuffer vertsI
  withVAO pV $ drawArrays Patches 0 (fromIntegral $ length vertsI)

  currentProgram $= Just (program vS)
  setUniform vS "view" view
  setUniform vS "proj" proj
  setUniform vS "model" eye
  setUniform vS "zoom" zoom
  setUniform vS "arrowrot" arrowrot
  setUniform vS "arrowscale" arrowscale
  s <- get bds
  let verts = fmap realToFrac <$> concatMap bodyVertices s :: [V3 DT]
  bindBuffer ArrayBuffer $= Just vB
  replaceBuffer ArrayBuffer verts
  withVAO vV $ drawArrays Lines 0 (fromIntegral $ length verts)

  throwError -- get errors >>= (\e -> unless (null e) $ print e)

  G.swapBuffers w
  G.pollEvents
