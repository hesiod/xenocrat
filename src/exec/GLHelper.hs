module GLHelper where


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

import Constants
import RenderGL
import Simulation
import Common

black, white, blue, red, green :: Color4 DT
black = Color4 0 0 0 1
white = Color4 1 1 1 1
blue = Color4 0 0 1 1
red = Color4 1 0 0 1
green = Color4 0 1 0 1


data ProgramIdentifier = Cross | Planet | Vector deriving (Ord, Eq)
type SBV = (ShaderProgram, Map BufferTarget BufferObject , VAO)
type MSBV = Map ProgramIdentifier SBV
type GLState = (MSBV, IORef (Camera DT), IORef (State SI (Pair FT)), IORef (Screen GLfloat))
