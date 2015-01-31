module Constants where

import Data.Metrology

import Common

type FT = Float

ae :: FT
ae = 1.4960e11
moonPeriod :: FT
moonPeriod = 2.360448e6

type Body2 = Body (FT, FT)

earth :: Body2
earth = Body 5.974e24 (0,0) (0,0)--29.7867e3)
moon :: Body2
moon = Body 7.349e22 (3.844e8,0) (0,1e3) -- +29.7867e3)
sun :: Body2
sun = Body 1.989e30 (-ae,0) (0,0)
