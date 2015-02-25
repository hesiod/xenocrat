{-# LANGUAGE TemplateHaskell #-}

module Shaders where

import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Search as BSS
import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects

passthroughVS, defaultVS, planetVS, planetTCS, planetTES, crossGS, vectorGS, defaultFS :: (ShaderType, BS.ByteString)
passthroughVS = (VertexShader, $(embedFile "src/shader/passthrough.vert"))
defaultVS = (VertexShader, $(embedFile "src/shader/default.vert"))
planetVS = (VertexShader, $(embedFile "src/shader/planet.vert"))
planetTCS = (TessControlShader, $(embedFile "src/shader/planet.tctrl"))
planetTES = (TessEvaluationShader, $( [| BL.toStrict $ BSS.replace (BC.pack "#insert noise3D") $(embedFile "src/shader/glsl-noise/src/noise3D.glsl") $(embedFile "src/shader/planet.teval") |] ))
crossGS = (GeometryShader, $(embedFile "src/shader/cross.geom"))
vectorGS = (GeometryShader, $(embedFile "src/shader/vector.geom"))
defaultFS = (FragmentShader, $(embedFile "src/shader/default.frag"))
