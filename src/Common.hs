{-# LANGUAGE StandaloneDeriving, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Common where

import Data.VectorSpace

data Body v = Body { mass :: Scalar v, pos :: v, vel :: v }
deriving instance (Eq v, Eq (Scalar v)) => Eq (Body v)
deriving instance (Show v, Show (Scalar v)) => Show (Body v)
