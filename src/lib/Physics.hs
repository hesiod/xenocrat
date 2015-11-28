{-# LANGUAGE TypeFamilies, TypeOperators, ScopedTypeVariables, DataKinds, FlexibleContexts, ConstraintKinds #-}

module Physics where

import Control.Monad.Reader
import Control.DeepSeq
import Data.List
import Data.Metrology.SI.Poly
import Data.Metrology.Poly
import Data.Metrology.Linear
import Linear.Metric
import Linear.Affine
--import Data.Metrology.SI.PolyTypes
import Common
import Constants

fG :: forall f a. (Metric f, Metric (Diff f), Floating a, Num (f a)) => Body SI f a -> Body SI f a -> Force SI (f a)
fG a b = dir |^*| f
    where
      dir = qSignorm $ pos b |-| pos a
      m = gamma |*| mass a |*| mass b
      r = qQd (pos b) (pos a)
      f = redim $ m |/| r :: Force SI a
--      dir = qNormalized $ pos a |.-.| pos b :: Qu '[] SI v

fA :: (Functor t, Foldable t, Metric f, Metric (Diff f), Floating a, Num (f a)) => Body SI f a -> t (Body SI f a) -> Force SI (f a)
fA ref bodies = qSum . fmap (fG ref) $ bodies

vA :: (Functor t, Foldable t, Metric f, Metric (Diff f), Num (f a), Floating a) => Body SI f a -> t (Body SI f a) -> Reader (Time SI a) (Velocity SI (f a))
vA ref bodies = do
  dt <- ask
  let ffA = fA ref bodies
      v = (ffA |^*| dt) |^/| mass ref
  return $ vel ref |+| v

dP :: (Functor t, Foldable t, Metric f, Metric (Diff f), Num (f a), Floating a) => Body SI f a -> t (Body SI f a) -> Reader (Time SI a) (Body SI f a)
dP ref bodies = do
  dt <- ask
  v1 <- vA ref bodies
  let v0 = vel ref
      vAvg = (v0 |+| v1) |^/| 2
  return $ ref { vel = v1, pos = (vAvg |^*| dt) |+| pos ref }

updateState :: forall f a. (Metric f, Metric (Diff f), Eq a, Floating a, Eq (f a), Num (f a), NFData (f a), NFData a) => [Body SI f a] -> Reader (Time SI a) [Body SI f a]
updateState st = mapM update st
    where
      update :: Body SI f a -> Reader (Time SI a) (Body SI f a)
      update b = dP b $ delete b st
