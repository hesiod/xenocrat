{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Render where

import Graphics.Gloss
import Debug.Trace

import Data.Monoid
import Data.VectorSpace
import Data.Basis

import Physics
import Common

type State v = [Body v]
type Zoom s = (s, s)

scaleCoordinates :: (HasBasis v, AdditiveGroup v, Show s, Fractional s, s ~ Scalar v) => s -> v -> (s, s)
scaleCoordinates zoom v = trace ("x:y " ++ show x ++ " " ++ show y) (x, y)
    where
      c = map snd . decompose . (^/zoom) $ v
      (x:y:_) = c

picturizeV :: (HasBasis v, AdditiveGroup v, s ~ Float, s ~ Scalar v) => Zoom s -> Body v -> Picture
picturizeV zoom b = Translate xR yR $ mconcat [cir, li, slir, slil]
    where
      (zoomR, zoomV) = zoom
      (xR, yR) = scaleCoordinates zoomR (pos b)
      (xV, yV) = scaleCoordinates zoomV (vel b)
      cir = Color black $ circleSolid 5
      li = Color blue $ Line [(xV, yV), (0, 0)]
      fac = 0.07
      deg = 135 + 10
      sli = Scale fac fac $ li
      slir = Color blue $ Translate xV yV $ Rotate deg sli
      slil = Color blue $ Translate xV yV $ Rotate (-deg) sli

picturizeState :: (HasBasis v, AdditiveGroup v, s ~ Float, s ~ Scalar v) => Zoom s -> State v -> Picture
picturizeState zoom s = mconcat . map (picturizeV zoom) $ s

updateState :: forall v s. (InnerSpace v, Floating s, Eq s, Eq v, s ~ Scalar v) => s -> State v -> State v
updateState dt st = map updatePoint st
    where
      updatePoint :: Body v -> Body v
      updatePoint body = let st' = filter (/= body) st in dP body st' dt
