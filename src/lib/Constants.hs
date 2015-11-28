{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, TemplateHaskell, FlexibleInstances, ConstraintKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}

module Constants where

import qualified Data.Dimensions.SI as D
import Data.Metrology.TH
import Data.Metrology.SI.Poly
import Data.Metrology.Poly
--import Data.Constants.Mechanics
import Linear.V3

import Common

$(declareDerivedUnit "AU" [t| Meter |] 1.4960e11 (Just "AU"))

earthVel :: (Fractional a) => Velocity SI a
earthVel = 29.7867e3 % (Meter :/ Second)

type instance DefaultUnitOfDim D.Mass = Kilo :@ Gram
type instance DefaultUnitOfDim D.Time = Second
type instance DefaultUnitOfDim D.Length = Meter
type instance DefaultUnitOfDim D.Current = Ampere
type instance DefaultUnitOfDim D.Temperature = Kelvin

declareConstant "gamma" 6.67384e-11 [t| (Meter :^ Three) :* ((Kilo :@ Gram) :^ MOne) :* (Second :^ MTwo) |]

earth, moon, sun :: forall a. (Fractional a) => Body SI V3 a
earth = let ma = 5.9721986e24 % (Kilo :@ Gram) :: Mass SI a;
            po = zero :: Length SI (V3 a);
            ve = V3 0 (earthVel # (Meter :/ Second)) 0 % (Meter :/ Second) :: Velocity SI (V3 a);
        in Body ma po ve
moon =  let ma = 7.3459e22 % (Kilo :@ Gram);
            po = V3 3.85e8 0 0 % Meter;
            ve = V3 0 (1e3 + (earthVel # (Meter :/ Second))) 0 % (Meter :/ Second);
        in Body ma po ve
sun =   let ma = 1.988435e30 % (Kilo :@ Gram);
            po = V3 1 0 0 % AU;
            ve = V3 0 0 0 % (Meter :/ Second) :: Velocity SI (V3 a);
        in Body ma po ve
