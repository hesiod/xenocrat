{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs, FlexibleInstances, DeriveGeneric, DeriveAnyClass, StandaloneDeriving, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
-- Safe
module Common where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Metrology.SI.Poly
import Data.Metrology.Unsafe
import Graphics.Rendering.OpenGL hiding (Length, Point)

type FT = GLfloat
type DT = GLdouble

-- pos should be a QPoint
data Body l f a = Body { mass :: Mass l a,
                         pos :: Length l (f a),
                         vel :: Velocity l (f a) }
                  deriving (Generic)
deriving instance (NFData a, NFData (f a)) => NFData (Body l f a)
deriving instance Generic (Qu u l v)
deriving instance NFData v => NFData (Qu u l v)
deriving instance (Eq (f a), Eq a, Functor f) => Eq (Body l f a)
deriving instance (Show (Mass l a), Show (Length l (f a)), Show (Velocity l (f a))) => Show (Body l f a)
