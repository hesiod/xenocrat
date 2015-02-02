module Constants where

import Data.Metrology
import Graphics.Rendering.OpenGL

import Common

type FT = GLfloat

ae :: FT
ae = 1.4960e11
moonPeriod :: FT
moonPeriod = 2.360448e6

type Body2 = Body (FT, FT)

earth, moon, sun :: Body2
earth = Body 5.974e24 (0,0) (0,0) --29.7867e3)
moon = Body 7.349e22 (3.844e8,0) (0,1e3) -- +29.7867e3)
sun = Body 1.989e30 (-ae,0) (0,0)
