{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds, ScopedTypeVariables #-}

module GLInit where

import Control.Concatenative
import Control.DeepSeq
import Control.Monad
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
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import Control.Concurrent
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import Foreign.Storable

import Constants
import Common

import Simulation
import GLHelper
import Shaders

import GL

dollySpeed :: Floating a => a
dollySpeed = recip 10
rotateSpeed :: Integral a => a
rotateSpeed = 1
keyboardTab :: (Conjugate a, Integral a, RealFloat a, Epsilon a) => Map GLFW.Key (Camera a -> Camera a)
keyboardTab = Map.fromList $ mov ++ rot
                  where
                    movKeys = [GLFW.Key'R, GLFW.Key'F, GLFW.Key'S, GLFW.Key'W, GLFW.Key'D, GLFW.Key'A]
                    rotKeys = [GLFW.Key'E, GLFW.Key'Q, GLFW.Key'Up, GLFW.Key'Down, GLFW.Key'Left, GLFW.Key'Right]
                    list = [V3 [0] [dollySpeed, negate dollySpeed] [0], V3 [0] [0] [dollySpeed, negate dollySpeed], V3 [dollySpeed, negate dollySpeed] [0] [0]]
                    mov = zip movKeys $ dolly <$> concatMap sequenceA list
                    rot = zip rotKeys $ do
                            f <- [roll, tilt, pan]
                            x <- [rotateSpeed, negate rotateSpeed]
                            return $ f x
keyboardC :: (Conjugate a, Integral a, RealFloat a, Epsilon a) => IORef (Camera a) -> GLFW.KeyCallback
keyboardC _ window GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose window True
keyboardC cam _ key _ GLFW.KeyState'Repeating _ = modifyIORef cam $ keyboardTab ! key
keyboardC _ _ _ _ _ _ = return ()

errorC :: GLFW.ErrorCallback
errorC _ = putStrLn

reshapeC :: Integral a => IORef (Screen a) -> GLFW.WindowSizeCallback
reshapeC screen _ w h = do
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  screen $= (fromIntegral w, fromIntegral h)
  print (w, h)

cursorC :: (Integral a, RealFloat a, Epsilon a) => IORef (Camera a) -> GLFW.CursorPosCallback --  Window -> Double -> Double -> IO ()
cursorC cam _ x y = modifyIORef cam (tilt dy . pan dx)
    where
      dx = fromIntegral dx
      dy = fromIntegral dy

windowHints :: IO ()
windowHints =
  mapM_ GLFW.windowHint [ GLFW.WindowHint'Samples 16,
                       GLFW.WindowHint'DepthBits 16,
                       GLFW.WindowHint'Decorated False,
                       GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
                       GLFW.WindowHint'ContextVersionMajor 4,
                       GLFW.WindowHint'ContextVersionMinor 5]

glInit :: IO ()
glInit = init
    where
      init = GLFW.setErrorCallback (Just errorC)
             >> GLFW.init
             >>= bool exitFailure init'
      init' = windowHints
              >> GLFW.createWindow 1024 768 "xenocrat" Nothing Nothing
              >>= maybe (GLFW.terminate >> exitFailure)
                  (triM_ (GLFW.makeContextCurrent . Just) setup GLFW.destroyWindow >=> const (GLFW.terminate >> exitSuccess))
      setup window = do
        st <- glSetup ([earth, moon, sun] :: State SI (Pair DT))
        GLFW.swapInterval 1
        GLFW.setKeyCallback window $ Just $ keyboardC $ cam st
        GLFW.setCursorPosCallback window $ Just $ cursorC $ cam st
        GLFW.setWindowRefreshCallback window $ Just $ displayState st
        GLFW.setWindowSizeCallback window $ Just $ reshapeC $ screen st

        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        depthFunc $= Just Less
        clearColor $= blue

        let interval = 1 % Second -- :: Time SI s
            delay = msDelay 100
        timer <- repeatedTimer GLFW.postEmptyEvent delay
        success <- repeatedRestart timer
        unless success $ GLFW.terminate >> exitFailure
        _ <- forkIO $ forever $ do
               b <- readIORef $ bds st
               writeIORef (bds st) . force . updateState interval $ b
        displayState st window
--(Storable a, RealFloat a, Epsilon a, Conjugate a, Fractional (Scalar a), Fractional (Scalar (Scalar a)), VectorSpace a, VectorSpace (Scalar a)) =>
glSetup :: forall a v. State SI v -> IO (GLState a)
glSetup b = do
    let prepare p = bindFragDataLocation p "outColor" $= 0
        setup s = makeVAO $ enableAttrib s "position" >> setAttrib s "position" ToFloat (VertexArrayDescriptor 3 Float 0 offset0)
    crossSP <- loadShaderProgramWithBS [passthroughVS, crossGS, defaultFS] prepare
    vectorSP <- loadShaderProgramWithBS [defaultVS, vectorGS, defaultFS] prepare
    planetSP <- loadShaderProgramWithBS [planetVS, planetTCS, planetTES, planetFS] prepare
    crossB <- makeBuffer ArrayBuffer [V3 0 0 0 :: V3 a, V3 1 1 1]
    crossV <- setup crossSP
    planetB <- makeBuffer ArrayBuffer [V3 0 0 0 :: V3 a]
    planetV <- setup planetSP
    vectorB <- makeBuffer ArrayBuffer (replicate 6 $ V3 0 0 0 :: [V3 a])
    vectorV <- setup vectorSP
    let sbv = Map.fromList [(Cross, (crossSP, Map.fromList [(ArrayBuffer, crossB)], crossV)),
                            (Vector, (vectorSP, Map.fromList [(ArrayBuffer, vectorB)], vectorV)),
                            (Planet, (planetSP, Map.fromList [(ArrayBuffer, planetB)], planetV))] :: MSBV

    bds <- newIORef b
    screen <- newIORef (1024, 768)
    cam <- newIORef (dolly (V3 0 0 1) fpsCamera :: Camera a)
    patchVertices $= 3
    lineWidth $= 0.1
    return $ GLState sbv cam bds screen
