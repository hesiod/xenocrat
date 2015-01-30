module Render where

import Graphics.Gloss
import GHC.Float
import Debug.Trace
import Data.Monoid

import Physics
import Common
import Vector

type State = [Body]
type Zoom = (Float, Float)

scaleCoordinates :: Float -> Vec -> (Float, Float)
scaleCoordinates zoom v = trace ("x:y " ++ show x' ++ " " ++ show y') (x'/zoom, y'/zoom)
    where
      (x':y':_) = map double2Float v

picturizeV :: Zoom -> Body -> Picture
picturizeV zoom b = Translate xR yR $ Pictures [circle, li, slir, slil]
    where
      (zoomR, zoomV) = zoom
      (xR, yR) = scaleCoordinates zoomR (pos b)
      (xV, yV) = scaleCoordinates zoomV (vel b)
      circle = Color black $ circleSolid 5
      li = Color blue $ Line [(xV, yV), (0, 0)]
      fac = 0.07
      deg = 135 + 10
      sli = Scale fac fac $ li
      slir = Color blue $ Translate xV yV $ Rotate deg sli
      slil = Color blue $ Translate xV yV $ Rotate (-deg) sli

picturizeState :: Zoom -> State -> Picture
picturizeState zoom s = mconcat . map (picturizeV zoom) $ s

updateState :: Double -> State -> State
updateState dt l = map updatePoint l
    where
      updatePoint :: Body -> Body
      updatePoint b = let l' = filter (/= b) l in dP b l' dt
