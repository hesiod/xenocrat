{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass, StandaloneDeriving, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Common where

import Data.VectorSpace
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Metrology.SI.Poly
import Data.Metrology.Unsafe
import Graphics.Rendering.OpenGL hiding (Length, Point)

type FT = GLdouble
type DT = GLfloat

-- pos should be a QPoint
data Body l v = Body { mass :: !(Mass l (Scalar v)), pos :: !(Length l v), vel :: !(Velocity l v) } deriving (Generic)
deriving instance (NFData v, NFData (Scalar v)) => NFData (Body l v)
deriving instance Generic (Qu u l v)
deriving instance NFData v => NFData (Qu u l v)
deriving instance (Eq v, Eq (Scalar v)) => Eq (Body l v)
deriving instance (Show (Mass l (Scalar v)), Show (Length l v), Show (Velocity l v)) => Show (Body l v)
