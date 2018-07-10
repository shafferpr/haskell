import Data.List

data DX a = DX { val :: a, dx :: DX a}

instance Num n => Num (DX n) where
  fromInteger x = DX (fromInteger x) 0
  DX x0 x' + DX y0 y' = DX (x0 + y0) (x' + y')
  DX x0 x' - DX y0 y' = DX (x0 - y0) (x' - y')
  x@(DX x0 x') * y@(DX y0 y') = DX (x0*y0) (x*y' + y*x')
  signum (DX x0 x') = DX (signum x0) 0
  abs x@(DX x0 x') = DX (abs x0) (signum x*x')


instance Fractional n => Fractional (DX n) where
  fromRational n = DX (fromRational n) 0
  x@(DX x0 x') / y@(DX y0 y') = DX (x0 / y0) ((x'*y - x*y')/ y ^ 2)

instance Eq a => Eq (DX a) where a == b = val a == val b
instance Ord a => Ord (DX a) where
  compare a b = compare (val a) (val b)

dZero :: Num a => DX a
dZero = DX 0 dZero

dConst :: Num a => a -> DX a
dConst x0 = DX x0 dZero

dlift :: Num a => (a -> a) -> (DX a -> DX a) -> DX a -> DX a
dlift f f' =  \ u@(DX u0 u') -> DX (f u0) (f' u * u')

instance Floating a => Floating (DX a) where
  pi    = dConst pi
  exp   = dlift exp exp
  sqrt  = dlift sqrt (recip . (2*) . sqrt)

var x = DX x 1

instance Show a => Show (DX a) where
  show (DX x (DX x' (DX x'' _))) = show [x, x', x'']

sqr x = convAbs $ iterate improve 1
  where improve r = (r + x / r) / 2
        convAbs (x₁ : x₂ : _) | abs (x₁ - x₂) < 1e-10 = x₂
        convAbs (_:xs) = convAbs xs

ljPotential :: (Floating a) => [a] -> [a] -> a
ljPotential xs ys = 1/radius^12 - 1/radius^6
  where radius =  sqrt $ sum $ map(^2) $ zipWith(-) xs ys

ljPotentialPeriodic :: (Floating a, Ord a) => [[a]] -> [a] -> [a] -> a
ljPotentialPeriodic boxvectors xs ys = 1/radius^12 - 1/radius^6
  where radius = sqrt $ sum $ map(^2) $ periodicVectors boxvectors xs ys

ljForcePeriodic :: (Floating a, Ord a) => [[a]] -> [a] -> [a] -> a
--periodicDiff :: (Floating a) => [[a]] -> [a] -> [a] -> [a]
--periodicDiff boxvectors x y =

periodicVectors :: (Floating a, Ord a) => [[a]] -> [a] -> [a] -> [a]
periodicVectors boxvectors x y = map minimumAbs $ zipWith(\q z -> z:(allPeriodicPairs q z) ) boxvectors $ zipWith(-) x y

periodicPair :: (Num a) => a -> a -> [a]
periodicPair x y = [x+y,x-y]

allPeriodicPairs :: (Num a) => [a] -> a -> [a]
allPeriodicPairs x y = concat $ map (\q -> periodicPair y q) x

maximumAbs :: (Num a, Ord a) => [a] -> a
maximumAbs x = maximumBy compareAbs x

minimumAbs :: (Num a, Ord a) => [a] -> a
minimumAbs x = minimumBy compareAbs x

compareAbs :: (Num a, Ord a) => a -> a -> Ordering
compareAbs x y = compare (abs x) (abs y)

forces :: (Floating a) => [[a]] -> [[a]]
forces [] = []
forces (x:[]) = [x]
forces (x:y:[]) = [pairForce x y, pairForce y x]
forces (x:xs) = (foldl(\acc q -> zipWith (+) acc $ pairForce x q) [0,0] xs):(addNewForces x xs $ forces xs)

addNewForces :: (Floating a) => [a] -> [[a]] -> [[a]] -> [[a]]
addNewForces x xs fxs = map (\q -> zipWith(+) (snd q) $ pairForce (fst q) x  ) $ zip xs fxs

pairForce :: (Floating a) => [a] -> [a] -> [a]
pairForce x y = zipWith (+) x y
