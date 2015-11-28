{-# LANGUAGE ScopedTypeVariables #-}

module GLHelper where

import Data.IORef
import Data.Metrology.Show()
import Data.Metrology.SI.Poly
import Data.Metrology.Poly
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import Linear

import Common

black, white, blue, red, green :: Num a => Color4 a
black = Color4 0 0 0 1
white = Color4 1 1 1 1
blue = Color4 0 0 1 1
red = Color4 1 0 0 1
green = Color4 0 1 0 1

data ProgramIdentifier = Cross | Planet | Vector deriving (Ord, Eq)
type SBV = (ShaderProgram, Map BufferTarget BufferObject, VAO)
type MSBV = Map ProgramIdentifier SBV


dollySpeed :: Floating a => a
dollySpeed = recip 10
rotateSpeed :: Num a => a
rotateSpeed = 1
keyboardTab :: (Conjugate a, RealFloat a, Epsilon a) => Map GLFW.Key (Camera a -> Camera a)
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
keyboardC :: (Conjugate a, RealFloat a, Epsilon a) => IORef (Camera a) -> GLFW.KeyCallback
keyboardC _ window GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose window True
keyboardC cam _ key _ GLFW.KeyState'Repeating _ = modifyIORef cam $ keyboardTab ! key
keyboardC _ _ _ _ _ _ = return ()

errorC :: GLFW.ErrorCallback
errorC _ = putStrLn

reshapeC :: Integral a => IORef (V2 a) -> GLFW.WindowSizeCallback
reshapeC screen _ w h = do
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  screen $= V2 (fromIntegral w) (fromIntegral h)
  print (w, h)

cursorC :: (RealFloat a, Epsilon a) => IORef (Camera a) -> GLFW.CursorPosCallback
cursorC cam _ x y = modifyIORef cam (tilt dy . pan dx)
    where
      (dx, dy) = (0,0)-- (realToFrac *** realToFrac) (x, y)

windowHints :: IO ()
windowHints =
  mapM_ GLFW.windowHint [ GLFW.WindowHint'Samples 16,
                       GLFW.WindowHint'DepthBits 16,
                       GLFW.WindowHint'Decorated False,
                       GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core,
                       GLFW.WindowHint'ContextVersionMajor 4,
                       GLFW.WindowHint'ContextVersionMinor 5]

bodyVertices :: forall t f a. (Foldable t, Fractional (f a), Functor f, Fractional a) => t (Body SI f a) -> [f a]
bodyVertices = concatMap verts
    where
      verts :: Body SI f a -> [f a]
      verts b = [p, p + 5e5 * v]
          where
            p = pos b # Meter
            v = vel b # (Meter :/ Second)

conv :: (Real a, Functor f, Fractional (f a), Fractional a, Fractional b) => [Body SI f a] -> [f b]
conv = fmap (fmap realToFrac) . bodyVertices
