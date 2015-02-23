module GLHelper where

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Linear.V3
import Data.ByteString

black, white, blue, red, green :: Color4 GLfloat
black = Color4 0 0 0 1
white = Color4 1 1 1 1
blue = Color4 0 0 1 1
red = Color4 1 0 0 1
green = Color4 0 1 0 1

buildShader :: [(ShaderType, ByteString)] -> (Program -> IO ()) -> IO ShaderProgram
buildShader shaders prepare = do
    sp <- loadShaderProgramWithBS shaders prepare
    let p = program sp
    get (linkStatus p) >>= print
    get (validateStatus p) >>= print
    get (programInfoLog p) >>= print
    return sp

{-
Needs ExistentialQuantification
data Uniformlike = forall a. AsUniform a => U a
instance AsUniform Uniformlike where
    asUniform = asUniform
type Uniforms = [(String, Uniformlike)]
setUniforms :: ShaderProgram -> Uniforms -> IO ()
setUniforms sp us = mapM_ (uncurry (setUniform sp)) us
-}
