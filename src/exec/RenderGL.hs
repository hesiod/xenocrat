{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module RenderGL where

import Data.Metrology.SI.Poly
import Data.Metrology.Vector
import Control.Arrow
import Linear

import Common

bodyVertices :: Body SI (FT, FT) -> [V3 FT]
bodyVertices b = [ V3 xr yr 0, V3 xv yv 0 ]
    where
      (xr, yr) = pos b # Meter
      (xv, yv) = ((+xr) *** (+yr)) . ((*1e6) *** (*1e6)) $ vel b # Meter :/ Second
