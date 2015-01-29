module Common where

import Vector

data Body = Body { mass :: Double, pos :: Vec, vel :: Vec } deriving (Eq, Show)
