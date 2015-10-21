{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module GL where

import Control.Concatenative
import Control.DeepSeq
import Control.Monad
import Control.Concurrent
import Control.Applicative
import Data.IORef
import Data.Bool
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
                       G.WindowHint'DepthBits 16,
                       G.WindowHint'Decorated False,
                       G.WindowHint'OpenGLProfile G.OpenGLProfile'Core,
                       G.WindowHint'ContextVersionMajor 4,
                       G.WindowHint'ContextVersionMinor 5]

init :: (G.Window -> IO ()) -> IO ()
init main = G.setErrorCallback (Just errorC) >> G.init >>= bool exitFailure (windowHints >> G.createWindow 1024 768 "xenocrat" Nothing Nothing >>= maybe (G.terminate >> exitFailure) (triM_ (G.makeContextCurrent . Just) main G.destroyWindow >=> const (G.terminate >> exitSuccess)))

data ProgramIdentifier = Cross | Planet | Vector deriving (Ord, Eq)
type SBV = (ShaderProgram, Map BufferTarget BufferObject , VAO)
type MSBV = Map ProgramIdentifier SBV

glMain :: forall v s. (ConformingVector v, s ~ Scalar v, s ~ FT, v ~ (FT, FT)) => IO ()
glMain = GL.init (\window -> do
    let prepare p = bindFragDataLocation p "outColor" $= 0
        setup s = makeVAO $ enableAttrib s "position" >> setAttrib s "position" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)
    crossSP <- loadShaderProgramWithBS [passthroughVS, crossGS, defaultFS] prepare
    vectorSP <- loadShaderProgramWithBS [defaultVS, vectorGS, defaultFS] prepare
    planetSP <- loadShaderProgramWithBS [planetVS, planetTCS, planetTES, planetFS] prepare
    crossB <- makeBuffer ArrayBuffer [V3 0 0 0 :: V3 DT, V3 1 1 1]
    crossV <- setup crossSP
    planetB <- makeBuffer ArrayBuffer [V3 0 0 0 :: V3 DT]
    planetV <- setup planetSP
    vectorB <- makeBuffer ArrayBuffer (replicate 6 $ V3 0 0 0 :: [V3 DT])
    vectorV <- setup vectorSP
    let sbv = Map.fromList [(Cross, (crossSP, Map.fromList [(ArrayBuffer, crossB)], crossV)),
                            (Vector, (vectorSP, Map.fromList [(ArrayBuffer, vectorB)], vectorV)),
                            (Planet, (planetSP, Map.fromList [(ArrayBuffer, planetB)], planetV))] :: MSBV

    bds <- newIORef [earth, moon, sun]
    screen <- newIORef (1024, 768)
    cam <- newIORef (dolly (V3 0 0 1) fpsCamera :: Camera DT)
    patchVertices $= 3
    lineWidth $= 0.1
    let display = displayState sbv cam bds screen
        reshape = reshapeC screen
    G.swapInterval 1
    G.setKeyCallback window $ Just $ keyboardC cam
    G.setWindowRefreshCallback window $ Just display
    G.setWindowSizeCallback window $ Just reshape

    _ <- forkIO . forever $ readIORef bds >>= writeIORef bds . force . updateState (10 % Second :: Time SI s)
    display window
    )

displayState :: MSBV -> IORef (Camera DT) -> IORef (State SI (Pair FT)) -> IORef (Screen GLfloat) -> G.WindowRefreshCallback
displayState sbv cam bds screen w = forever $ do
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  depthFunc $= Just Less
  clearColor $= blue
  clear [ColorBuffer, DepthBuffer, StencilBuffer]

  camera <- get cam
  scr <- get screen

  let (cS, cBs, cV) = sbv ! Cross
      (pS, pBs, pV) = sbv ! Planet
      (vS, vBs, vV) = sbv ! Vector
      cB = cBs ! ArrayBuffer
      pB = pBs ! ArrayBuffer
      vB = vBs ! ArrayBuffer

      zoom = recip 3e11 :: DT
      arrowscale = 0.6 :: DT
      tessI = 8*8 :: DT
      tessO = 8*8 :: DT
      noisiness = 0.2 :: DT

      mkRot axis rad = m33_to_m44 . fromQuaternion $ axisAngle axis rad
      arrowrot = mkRot (V3 0 0 1) (deg2rad 50) :: M44 DT
      scaleM = m33_to_m44 (identity !!* zoom)
      model = camMatrix camera
      view = Linear.lookAt (V3 0 0 0) (V3 0 0 (-1)) (V3 0 1 0)
      near = 0.0001
      far = 10
      fov = 0.8
      ar = uncurry (/) scr
      proj = Linear.perspective fov ar near far
      transform = proj !*! view !*! model :: M44 DT

  currentProgram $= Just (program cS)
  setUniform cS "transform" transform
  bindBuffer ArrayBuffer $= Just cB
  withVAO cV $ drawArrays Points 0 2

  currentProgram $= Just (program pS)
  setUniform pS "transform" transform
  setUniform pS "tess_inner" tessI
  setUniform pS "tess_outer" tessO
  setUniform pS "noisiness" noisiness
  setUniform pS "origin" $ V3 0 0 (0::DT)
  let vertsI = fmap realToFrac <$> icosahedronTriangles :: [V3 DT]
  bindBuffer ArrayBuffer $= Just pB
  replaceBuffer ArrayBuffer vertsI
  withVAO pV $ drawArrays Patches 0 (fromIntegral $ length vertsI)

  currentProgram $= Just (program vS)
  setUniform vS "transform" $ transform !*! scaleM
  setUniform vS "arrowrot" arrowrot
  setUniform vS "arrowscale" arrowscale
  s <- get bds
  let verts = fmap realToFrac <$> concatMap bodyVertices s :: [V3 DT]
  bindBuffer ArrayBuffer $= Just vB
  replaceBuffer ArrayBuffer verts
  withVAO vV $ drawArrays Lines 0 (fromIntegral $ length verts)

  throwError

  G.swapBuffers w
  G.pollEvents
