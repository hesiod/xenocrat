module Vector where
    
x v = v !! 0
y v = v !! 1
z v = v !! 2   

bmap = zipWith
amap :: Num a => (a -> a -> a) -> [a] -> a -> [a]
amap f a b = map (`f` b) a
             
distanceSqrd a b = sum . map (^2) $ a |- b
--distance = sqrt distanceSqrd

len :: (Floating a) => [a] -> a
len a = sqrt $ distanceSqrd (repeat 0) a

normalize :: (Floating a) => [a] -> [a]
normalize a = a ||/ len a
        
(|+) :: (Num a) => [a] -> [a] -> [a]
(|+) = bmap (+)
(|-) :: (Num a) => [a] -> [a] -> [a]
(|-) = bmap (-)
(|*) :: (Num a) => [a] -> [a] -> [a]
(|*) = bmap (*)
(|/) :: (Fractional a) => [a] -> [a] -> [a]
(|/) = bmap (/)
(||*) :: (Fractional a) => [a] -> a -> [a]
(||*) = amap (*)
(||/) :: (Fractional a) => [a] -> a -> [a]
(||/) = amap (/)

