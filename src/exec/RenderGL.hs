{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators, ConstraintKinds, TemplateHaskell #-}

module RenderGL where

import qualified Language.Haskell.TH.Lift as THL
import Instances.TH.Lift ()
import Foreign.C.Types (CFloat, CDouble)

import Prelude hiding (sequence)
import Data.Word
import Control.Arrow
import Control.Monad (replicateM)
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Linear
import Linear.V3

phi :: Floating a => a
phi = (1 + sqrt 5) / 2
silverRatio :: Floating a => a
silverRatio = recip phi
icosahedronTriangleArea :: Floating a => a
icosahedronTriangleArea = sqrt 3

THL.deriveLiftMany [''CFloat, ''CDouble, ''Linear.V3.V3]

{-
(0, ±1, ±φ)
(±1, ±φ, 0)
(±φ, 0, ±1)
-}
icosahedron :: (Eq a, Floating a) => [V3 a]
icosahedron = $( [| let b i = [ i, negate i ];
                          pm (V3 x y z) = sequenceA $ V3 (b x) (b y) (b z)
                      in nub . concatMap pm $ [ V3 0 1 phi, V3 1 phi 0, V3 phi 0 1 ] |] )

triangleArea :: (Floating a) => [V3 a] -> a
triangleArea (a:b:c:_) = let ab = b - a
                             ac = c - a
                         in 0.5 * norm (ab `cross` ac)
triangleArea _ = undefined

primitiveEquals :: Eq a => [a] -> [a] -> Bool
a `primitiveEquals` b = $( [| a `elem` permutations b |] )

-- 'Golden Values': 1.902113032590307 1.7320508075688772 4.534567884457024
icosahedronTriangles :: (Eq a, Floating a) => [V3 a]
icosahedronTriangles = $( [| concat . filter (\x -> triangleArea x == icosahedronTriangleArea) . nubBy primitiveEquals . replicateM 3 $ icosahedron |] )

makeIndices :: forall a i. (Ord a, Bounded i, Enum i) => [a] -> ([i], [a])
makeIndices = second (map fst . Map.toList) . discard3 . foldl reduce ([], Map.empty, minBound)
    where
      discard3 (a, b, _) = (a, b)
      reduce :: ([i], Map a i, i) -> a -> ([i], Map a i, i)
      reduce (indices, m, nextidx) v = if v `Map.member` m
                                       then let i = m ! v in (i : indices, m, nextidx)
                                       else (nextidx : indices, Map.insert v nextidx m, succ nextidx)

icosahedronIndices :: (Eq a, Floating a, Ord a) => ([Word32], [V3 a])
icosahedronIndices = $( [| makeIndices icosahedronTriangles |] )
