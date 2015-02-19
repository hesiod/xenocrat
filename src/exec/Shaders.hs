{-# LANGUAGE TemplateHaskell #-}

module Shaders where

import Data.FileEmbed
import Data.ByteString
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects

passthroughVS, defaultVS, crossGS, vectorGS, defaultFS :: (ShaderType, ByteString)
passthroughVS = (VertexShader, $(embedFile "src/shader/passthrough.vert"))
defaultVS = (VertexShader, $(embedFile "src/shader/default.vert"))
crossGS = (GeometryShader, $(embedFile "src/shader/cross.geom"))
vectorGS = (GeometryShader, $(embedFile "src/shader/vector.geom"))
defaultFS = (FragmentShader, $(embedFile "src/shader/default.frag"))
