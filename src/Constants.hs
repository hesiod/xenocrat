module Constants where

import Common

gamma :: Double
gamma = 6.6738e-11
ae :: Double
ae = 1.4960e11
moonPeriod :: Double
moonPeriod = 2.360448e6

earth :: Body
earth = Body 5.974e24 [0,0] [0,29.7867e3]
moon :: Body
moon = Body 7.349e22 [3.844e8,0] [0,29.7867e3+1e3]
sun :: Body
sun = Body 1.989e30 [-ae,0] [0,0]
