{-# LANGUAGE DeriveGeneric, StandaloneDeriving, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Common where

import Data.VectorSpace
import GHC.Generics (Generic)
import Control.DeepSeq

data Body v = Body { mass :: Scalar v, pos :: v, vel :: v } deriving Generic
instance NFData v => NFData (Body v)
deriving instance (Eq v, Eq (Scalar v)) => Eq (Body v)
deriving instance (Show v, Show (Scalar v)) => Show (Body v)
