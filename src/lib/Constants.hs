{-# LANGUAGE FlexibleContexts, TemplateHaskell, FlexibleInstances, ConstraintKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}

module Constants where

import Data.Metrology.Vector
import qualified Data.Dimensions.SI as D
import Data.Metrology.TH
import Data.Metrology.SI.Poly
import Data.VectorSpace
import Data.VectorSpace.OpenGL()

import Common

$(declareDerivedUnit "AU" [t| Meter |] 1.4960e11 (Just "AU"))

earthVel :: (Fractional a, VectorSpace a, Fractional (Scalar a)) => Velocity SI a
earthVel = 29.7867e3 % (Meter :/ Second)

type instance DefaultUnitOfDim D.Mass = Kilo :@ Gram
type instance DefaultUnitOfDim D.Time = Second
type instance DefaultUnitOfDim D.Length = Meter
type instance DefaultUnitOfDim D.Current = Ampere
type instance DefaultUnitOfDim D.Temperature = Kelvin

declareConstant "gamma" 6.67384e-11 [t| (Meter :^ Three) :* ((Kilo :@ Gram) :^ MOne) :* (Second :^ MTwo) |]

earth :: forall a. (Fractional a, Num a, VectorSpace a) => Body SI (a, a)
earth = let ma = 5.9721986e24 % (Kilo :@ Gram) :: Mass SI (Scalar a);
            po = zero;
            ve = (0, earthVel # (Meter :/ Second)) % (Meter :/ Second);
        in Body ma po ve
{-moon =  let ma = 7.3459e22 % (Kilo :@ Gram);
            po = (3.85e8, 0) % Meter;
            ve = (0, 1e3 + (earthVel # (Meter :/ Second))) % (Meter :/ Second);
        in Body ma po ve
sun =   let ma = 1.988435e30 % (Kilo :@ Gram);
            po = (1, 0) % AU;
            ve = (0, 0) % (Meter :/ Second);
        in Body ma po ve
-}
