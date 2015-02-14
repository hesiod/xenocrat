{-# LANGUAGE TypeSynonymInstances, DeriveGeneric, StandaloneDeriving, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Common where

import Data.VectorSpace
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Metrology.SI.Poly
import Graphics.UI.GLUT hiding (Length, Point)

type FT = GLdouble

-- pos should be a QPoint
data Body l v = Body { mass :: !(Mass l (Scalar v)), pos :: !(Length l v), vel :: !(Velocity l v) } deriving Generic
instance NFData v => NFData (Body l v)
instance NFData FT
deriving instance (Eq v, Eq (Scalar v)) => Eq (Body l v)
deriving instance (Show (Mass l (Scalar v)), Show (Length l v), Show (Velocity l v)) => Show (Body l v)
