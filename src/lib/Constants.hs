{-# LANGUAGE FlexibleInstances, ConstraintKinds, TypeOperators, TypeFamilies #-}

module Constants where

import Data.Metrology.Vector
import qualified Data.Dimensions.SI as D
import Data.Metrology.SI.Poly
import Data.VectorSpace.OpenGL()

import Common

ae :: Length SI FT
ae = 1.4960e11 % Meter
--moonPeriod :: FT
--moonPeriod = 2.360448e6

type instance DefaultUnitOfDim D.Mass = Kilo :@ Gram
type instance DefaultUnitOfDim D.Time = Second
type instance DefaultUnitOfDim D.Length = Meter
type instance DefaultUnitOfDim D.Current = Ampere
type instance DefaultUnitOfDim D.Temperature = Kelvin

type GammaU = (Meter :^ Three) :* ((Kilo :@ Gram) :^ MOne) :* (Second :^ MTwo)
gamma :: DefaultConvertibleLCSU_U GammaU l => MkQu_ULN GammaU l FT
gamma = constant $ 6.67384e-11 % (undefined :: GammaU)

earth, moon, sun :: Body SI (FT, FT)
earth = let ma = 5.9721986e24 % (Kilo :@ Gram);
            po = (0, 0) % Meter;
            ve = (0, 29.7867e3) % (Meter :/ Second);
        in Body ma po ve
moon =  let ma = 7.3459e22 % (Kilo :@ Gram);
             po = (3.85e8, 0) % Meter;
             ve = (0, 1e3+29.7867e3) % (Meter :/ Second);
        in Body ma po ve
sun =   let ma = 1.988435e30 % (Kilo :@ Gram);
             po = (qNegate ae # Meter, 0) % Meter;
             ve = (0, 0) % (Meter :/ Second);
        in Body ma po ve
