module GLHelper where

import Data.IORef
import Data.Metrology.Show()
import Data.Metrology.SI.Poly
import Data.Map (Map)
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL

import Simulation
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
data GLState a = GLState {
      msbv :: MSBV,
      cam :: IORef (Camera a),
      bds :: IORef (State SI (Pair a)),
      screen :: IORef (Screen Int) }
--data GLState <dp sc = (MSBV, IORef (Camera DT), IORef (State SI (Pair DT)), IORef (Screen Int))
