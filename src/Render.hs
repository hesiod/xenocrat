module Render where

import Physics
import Aux
import Graphics.Gloss
import GHC.Float
import Debug.Trace

type State = [Body]

rP :: FT -> Float -> Body -> Picture
rP dt zoom b = Color (makeColor 0 0 0 1) $ Translate x y $ circleSolid 1
          where
            (x':y':_) = map double2Float $ p b
            (x, y) = trace ("x:y " ++ (show x') ++ " " ++ (show y')) (x'/zoom, y'/zoom)
                     
rPs :: FT -> Float -> State -> Picture
rPs dt zoom s = Pictures $ map (rP dt zoom) s
                                          
update :: FT -> State -> State
update dt l = map uP l
    where
      uP :: Body -> Body
      uP b = let l' = filter (/= b) l in dP b l' dt
 
states :: FT -> Float -> State -> [Picture]
states dt zoom s = map (rPs dt zoom) sts
    where
      sts = iterate (update dt) s :: [State]
         
