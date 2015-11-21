{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module GLInit where

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
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import Control.Concurrent.Timer

import Constants
import Common

import RenderGL
import Simulation
import GLHelper
import Shaders

import GL

dollySpeed :: DT
dollySpeed = 0.1
rotateSpeed :: DT
rotateSpeed = 1
keyboardTab :: [(GLFW.Key, Camera DT -> Camera DT)]
keyboardTab = doll ++ seq
                  where
                    list = [V3 [0] [dollySpeed, negate dollySpeed] [0], V3 [0] [0] [dollySpeed, negate dollySpeed], V3 [dollySpeed, negate dollySpeed] [0] [0]]
                    dollKeys = [GLFW.Key'R, GLFW.Key'F, GLFW.Key'S, GLFW.Key'W, GLFW.Key'D, GLFW.Key'A]
                    doll = zip dollKeys (fmap dolly $ concatMap sequenceA list)
                    rotKeys = [GLFW.Key'E, GLFW.Key'Q, GLFW.Key'Up, GLFW.Key'Down, GLFW.Key'Left, GLFW.Key'Right]
                    seq = zip rotKeys $ do
                            f <- [roll, tilt, pan]
                            x <- [rotateSpeed, negate rotateSpeed]
                            return $ f x
keyboard :: IORef (Camera DT) -> GLFW.KeyCallback
keyboard _ window GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose window True
keyboard cam _ key _ GLFW.KeyState'Repeating _ = modifyIORef cam $ tab ! key
    where
      tab :: Map GLFW.Key (Camera DT -> Camera DT)
      tab = Map.fromList keyboardTab
keyboard _ _ _ _ _ _ = return ()
keyboardC :: IORef (Camera DT) -> GLFW.KeyCallback
keyboardC _ window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = GLFW.setWindowShouldClose window True
keyboardC cam _ GLFW.Key'R _ GLFW.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 0 dollySpeed 0)
keyboardC cam _ GLFW.Key'F _ GLFW.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 0 (-dollySpeed) 0)
keyboardC cam _ GLFW.Key'A _ GLFW.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 (-dollySpeed) 0 0)
keyboardC cam _ GLFW.Key'D _ GLFW.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 dollySpeed 0 0)
keyboardC cam _ GLFW.Key'W _ GLFW.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 0 0 (-dollySpeed))
keyboardC cam _ GLFW.Key'S _ GLFW.KeyState'Repeating _ = modifyIORef cam $ dolly (V3 0 0 dollySpeed)
keyboardC cam _ GLFW.Key'Q _ GLFW.KeyState'Repeating _ = modifyIORef cam $ roll (-rotateSpeed)
keyboardC cam _ GLFW.Key'E _ GLFW.KeyState'Repeating _ = modifyIORef cam $ roll rotateSpeed
keyboardC cam _ GLFW.Key'Up _ GLFW.KeyState'Repeating _ = modifyIORef cam $ tilt rotateSpeed
keyboardC cam _ GLFW.Key'Down _ GLFW.KeyState'Repeating _ = modifyIORef cam $ tilt (-rotateSpeed)
keyboardC cam _ GLFW.Key'Left _ GLFW.KeyState'Repeating _ = modifyIORef cam $ pan (-rotateSpeed)
keyboardC cam _ GLFW.Key'Right _ GLFW.KeyState'Repeating _ = modifyIORef cam $ pan rotateSpeed
keyboardC _ _ _ _ _ _ = return ()

errorC :: GLFW.ErrorCallback
errorC _ = putStrLn

reshapeC :: IORef (Screen DT) -> GLFW.WindowSizeCallback
reshapeC screen _ w h = do
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  screen $= (fromIntegral w, fromIntegral h)
  print (w, h)

windowHints :: IO ()
windowHints =
  mapM_ GLFW.windowHint [ GLFW.WindowHint'Samples 16,
                       GLFW.WindowHint'DepthBits 16,
                       GLFW.WindowHint'Decorated False,
                       GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
                       GLFW.WindowHint'ContextVersionMajor 4,
                       GLFW.WindowHint'ContextVersionMinor 5]

glInit :: forall v s. (ConformingVector v, s ~ Scalar v, s ~ FT, v ~ (FT, FT)) => IO ()
glInit = GLFW.setErrorCallback (Just errorC) >> GLFW.init >>= bool exitFailure (windowHints >> GLFW.createWindow 1024 768 "xenocrat" Nothing Nothing >>= maybe (GLFW.terminate >> exitFailure) (triM_ (GLFW.makeContextCurrent . Just) setup GLFW.destroyWindow >=> const (GLFW.terminate >> exitSuccess)))
    where
      setup window = do
        putStrLn "hello"
        st <- glSetup window
        let (_,cam, bds, scr) = st
        GLFW.swapInterval 1
        GLFW.setKeyCallback window $ Just $ keyboard cam
        GLFW.setWindowRefreshCallback window $ Just $ displayState st
        GLFW.setWindowSizeCallback window $ Just $ reshapeC scr

        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        depthFunc $= Just Less
        clearColor $= blue
        putStrLn "hello"

        let interval = 1 % Second :: Time SI s --_ <- forkIO $
        atomicModifyIORef' bds (\x -> (updateState interval x, ()))         --readIORef bds >>= writeIORef bds . force . updateState interval
        displayState st window
--      sc (_,_,_,s) = s


glSetup :: forall v s. (ConformingVector v, s ~ Scalar v, s ~ FT, v ~ (FT, FT)) => GLFW.Window -> IO GLState
glSetup window = do
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
    return (sbv, cam, bds, screen)
