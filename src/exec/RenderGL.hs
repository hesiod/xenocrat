{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds #-}

module RenderGL where

import Prelude hiding (sequence)
import Control.Arrow
import Data.Metrology.SI.Poly
import Data.Metrology.Vector
import Data.Traversable
import Data.List
import Linear
import Common

phi :: Floating a => a
phi = (1 + sqrt 5) / 2
silverRatio :: Floating a => a
silverRatio = 1 / phi

{-
(0, ±1, ±φ)
(±1, ±φ, 0)
(±φ, 0, ±1)
-}
icosahedron :: [V3 FT]
icosahedron = nub . concat . fmap pm $ [ V3 0 1 phi, V3 1 phi 0, V3 phi 0 1 ]
    where
      pm (V3 x y z) = let b i = [ i, negate i ] in sequenceA $ V3 (b x) (b y) (b z)

icosahedronTriangles :: [V3 FT]
icosahedronTriangles = concat . filter (\x -> area x == phiArea) . nubBy reduce $ triangles
    where
      phiArea = 5**(1/4) / sqrt silverRatio
      triangles = sequence [icosahedron, icosahedron, icosahedron]
      area (a:b:c:_) = 0.5 * norm $ (b - a) `cross` (c - a)
      area _ = undefined
      a `reduce` b = a `elem` permutations b -- group . sort . fmap area

bodyVertices :: Body SI (FT, FT) -> [V3 FT]
bodyVertices b = [ V3 xr yr 0, V3 xv yv 0 ]
    where
      (xr, yr) = pos b # Meter
      (xv, yv) = ((+xr) *** (+yr)) . ((*1e6) *** (*1e6)) $ vel b # Meter :/ Second
