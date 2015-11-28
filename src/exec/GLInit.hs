module GLInit where

import Control.Monad.Reader
import Control.Concatenative
import Control.DeepSeq
import Data.IORef
import Data.Metrology.Show()
import Data.Metrology.SI.Poly
import Data.Metrology.Poly
import qualified Data.Map as Map
import Linear
import Linear.Affine
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import Control.Concurrent
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted

import Constants
import Common

import Physics
import GLHelper
import Shaders

import GL

glInit :: IO ()
glInit = do
  GLFW.setErrorCallback (Just errorC)
  success <- GLFW.init
  unless success exitFailure
  windowHints
  let f = GLFW.terminate
          >> exitFailure
      s = triM_ (GLFW.makeContextCurrent . Just) glMain GLFW.destroyWindow
          >=> const (GLFW.terminate >> exitSuccess)
  GLFW.createWindow 1024 768 "xenocrat" Nothing Nothing >>= maybe f s

glMain :: GLFW.Window -> IO ()
glMain window = do
  crossSP <- loadShaderProgramWithBS [passthroughVS, crossGS, defaultFS] prepare
  vectorSP <- loadShaderProgramWithBS [defaultVS, vectorGS, defaultFS] prepare
  planetSP <- loadShaderProgramWithBS [planetVS, planetTCS, planetTES, planetFS] prepare
  crossB <- makeBuffer ArrayBuffer [nil, one]
  crossV <- setup crossSP
  planetB <- makeBuffer ArrayBuffer [nil]
  planetV <- setup planetSP
  vectorB <- makeBuffer ArrayBuffer $ replicate 6 nil
  vectorV <- setup vectorSP
  let sbv = Map.fromList [(Cross, (crossSP, Map.fromList [(ArrayBuffer, crossB)], crossV)),
                          (Vector, (vectorSP, Map.fromList [(ArrayBuffer, vectorB)], vectorV)),
                          (Planet, (planetSP, Map.fromList [(ArrayBuffer, planetB)], planetV))] :: MSBV
  patchVertices $= 3
  lineWidth $= 0.1

  let s = V2 1024 768
      c = dolly (V3 0 0 1) fpsCamera :: Camera FT
      b = [earth, moon, sun] :: [Body SI V3 DT]
  screen <- newIORef s
  cam <- newIORef c
  bds <- newIORef b
  verts <- newIORef $ conv b
  GLFW.swapInterval 1
  GLFW.setKeyCallback window $ Just $ keyboardC cam
  GLFW.setCursorPosCallback window $ Just $ cursorC cam
  GLFW.setWindowRefreshCallback window $ Just $ displayState verts screen sbv cam
  GLFW.setWindowSizeCallback window $ Just $ reshapeC screen

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  depthFunc $= Just Less
  clearColor $= blue

  timer <- repeatedTimer GLFW.postEmptyEvent delay
  success <- repeatedRestart timer
  unless success $ GLFW.terminate >> exitFailure
  _ <- forkIO $ compute bds verts
  displayState verts screen sbv cam window
    where
      prepare p = bindFragDataLocation p "outColor" $= 0
      setup s = makeVAO $ enableAttrib s "position" >> setAttrib s "position" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)
      nil = V3 0 0 0 :: V3 FT
      one = V3 1 1 1 :: V3 FT
      delay = msDelay 100

compute :: forall f a b. (NFData (f b), Fractional b, NFData a, NFData (f a), Metric f, Metric (Diff f), Eq a, Real a, Floating a, Fractional (f a), Eq (f a), Functor f) =>
           IORef [Body SI f a] -> IORef [f b] -> IO ()
compute bds verts = get bds >>= comp >> return ()
    where
      interval = 1 % Second
      comp :: [Body SI f a] -> IO [Body SI f a]
      comp b = do
        let b' = force $ runReader (updateState b) interval
            v  = force $ conv b'
        writeIORef bds b'
        writeIORef verts v
        comp b'
