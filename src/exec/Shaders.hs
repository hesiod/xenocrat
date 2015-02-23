{-# LANGUAGE TemplateHaskell #-}

module Shaders where

import Data.FileEmbed
import Data.ByteString
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects

passthroughVS, defaultVS, planetVS, planetTCS, planetTES, crossGS, vectorGS, defaultFS :: (ShaderType, ByteString)
passthroughVS = (VertexShader, $(embedFile "src/shader/passthrough.vert"))
defaultVS = (VertexShader, $(embedFile "src/shader/default.vert"))
planetVS = (VertexShader, $(embedFile "src/shader/planet.vert"))
planetTCS = (TessControlShader, $(embedFile "src/shader/planet.tctrl"))
planetTES = (TessEvaluationShader, $(embedFile "src/shader/planet.teval"))
crossGS = (GeometryShader, $(embedFile "src/shader/cross.geom"))
vectorGS = (GeometryShader, $(embedFile "src/shader/vector.geom"))
defaultFS = (FragmentShader, $(embedFile "src/shader/default.frag"))
