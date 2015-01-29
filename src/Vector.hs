module Vector where

type Vec = [Double]

x :: [a] -> a
x v = v !! 0
y :: [a] -> a
y v = v !! 1
z :: [a] -> a
z v = v !! 2

applyScalar :: Num a => (a -> a -> a) -> [a] -> a -> [a]
applyScalar f a b = map (`f` b) a

distanceSqrd :: Num a => [a] -> [a] -> a
distanceSqrd a b = sum . map (^exp2) $ a |- b
    where
      exp2 = 2 :: Int
--distance = sqrt distanceSqrd

len :: (Floating a) => [a] -> a
len a = sqrt $ distanceSqrd (repeat 0) a

normalize :: (Floating a) => [a] -> [a]
normalize a = a ||/ len a

(|+) :: (Num a) => [a] -> [a] -> [a]
(|+) = zipWith (+)
(|-) :: (Num a) => [a] -> [a] -> [a]
(|-) = zipWith (-)
(|*) :: (Num a) => [a] -> [a] -> [a]
(|*) = zipWith (*)
(|/) :: (Fractional a) => [a] -> [a] -> [a]
(|/) = zipWith (/)
(||*) :: (Fractional a) => [a] -> a -> [a]
(||*) = applyScalar (*)
(||/) :: (Fractional a) => [a] -> a -> [a]
(||/) = applyScalar (/)
