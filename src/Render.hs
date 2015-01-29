module Render where

import Physics
import Common
import Graphics.Gloss
import GHC.Float
import Debug.Trace

type State = [Body]

picturize :: Float -> Body -> Picture
picturize zoom b = Color (makeColor 0 0 0 1) $ Translate x y $ circleSolid 3
          where
            (x':y':_) = map double2Float $ pos b
            (x, y) = trace ("x:y " ++ show x' ++ " " ++ show y') (x'/zoom, y'/zoom)

picturizeState :: Float -> State -> Picture
picturizeState zoom s = Pictures $ map (picturize zoom) s

updateState :: FT -> State -> State
updateState dt l = map updatePoint l
    where
      updatePoint :: Body -> Body
      updatePoint b = let l' = filter (/= b) l in dP b l' dt

