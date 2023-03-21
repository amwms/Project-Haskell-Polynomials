{-# LANGUAGE InstanceSigs #-}
module SparsePoly(fromDP, toDP, qrP) where
import PolyClass
import Representation
import Distribution.Simple.Program.HcPkg (list)
import Distribution.Simple.Utils (xargs)

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a

fromDP = undefined
toDP = undefined

first :: (a -> a') -> (a, b) -> (a', b)
first = undefined
second :: (b -> b') -> (a, b) -> (a, b')
second = undefined

pairFirst :: (a, b) -> a
pairFirst (x, y) = x
pairSecond :: (a, b) -> b
pairSecond (x, y) = y

-- TODO 
sparseToCanonicalAndReverse :: (Eq a, Num a) => [a] -> [a]
sparseToCanonicalAndReverse xs = go [] xs where
    go list [] = list
    go list (x:xs)
        | pairSecond x == 0 = go list xs
        | otherwise = go (x:list) xs

addSamePower :: Num b => (a, b) -> (a, b) -> (a, b)
addSamePower x y = (pairFirst x, pairSecond x + pairSecond y)

instance Functor SparsePoly where

instance Polynomial SparsePoly where
    zeroP :: SparsePoly a
    zeroP = S []

    constP :: (Eq a, Num a) => a -> SparsePoly a
    constP x = S [(0, x)]

    varP   :: Num a => SparsePoly a                  -- p(x) = x
    varP = S [(1, 1)]

    evalP :: Num a => SparsePoly a -> a -> a        -- value of p(x) at given x
    evalP (S xs) x = go xs where
        go [] = 0
        go (h:xs) = pairSecond h * (x ^ pairFirst h) + go xs

    shiftP :: (Eq a, Num a) => Int -> SparsePoly a -> SparsePoly a -- multiply by x^n
    shiftP n (S xs) = S $ map (first (+n)) xs

    degree :: (Eq a, Num a) => SparsePoly a -> Int -- highest power with nonzero coefficient
    degree (S xs) = pairFirst $ head xs

instance (Eq a, Num a) => Num (SparsePoly a) where

    abs :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    abs x = undefined

    signum :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    signum x = undefined

    fromInteger :: (Eq a, Num a) => Integer -> SparsePoly a
    fromInteger x
        | x == 0 = S []
        | otherwise = S [(0, fromInteger x)]

    -- TODO - correct (+)
    (+) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> SparsePoly a
    (+) x y = S $ sparseToCanonicalAndReverse (go [] (unS x) (unS y)) where
        go list [] [] = list
        go list [] (y:ys) = go (y:list) [] ys
        go list (x:xs) [] = go (x:list) xs []
        go list (x:xs) (y:ys)
            | pairFirst x == pairFirst y = go ((addSamePower x y):list) xs ys
            | pairFirst x > pairFirst y = go (x:list) xs (y:ys)
            | otherwise = go (y:list) (x:xs) ys

    -- TODO (*)

    negate :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    negate x = S $ reverse (go [] (unS x)) where
        go list (x:xs) = go ((pairFirst x, negate (pairSecond x)):list) xs
        go list [] = list


instance (Eq a, Num a) => Eq (SparsePoly a) where
    (==) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> Bool
    p == q = nullP(p-q)

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP = undefined

-- | Division example
-- >>> qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True
