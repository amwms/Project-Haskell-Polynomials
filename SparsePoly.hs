{-# LANGUAGE InstanceSigs #-}
module SparsePoly(fromDP, toDP, qrP) where
import PolyClass
import Representation
import Data.List (sort, sortOn)

pairFirst :: (a, b) -> a
pairFirst (x, y) = x

pairSecond :: (a, b) -> b
pairSecond (x, y) = y

-- sparseToCanonical :: (Eq coef, Num coef) => [(Int, coef)] -> [(Int, coef)]
-- sparseToCanonical = filter (\x -> snd x /= 0)

listToCanonical :: (Eq coef, Num coef) => [(Int, coef)] -> [(Int, coef)]
listToCanonical = filter (\x -> snd x /= 0) . sortOn (negate . fst)

addSamePower :: Num coef => (Int, coef) -> (Int, coef) -> (Int, coef)
addSamePower x y = (pairFirst x, pairSecond x + pairSecond y)

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a

fromDP (P xs) = S $ listToCanonical $ zip [0..] xs

-- toDP (S xs) = sum $ map (\(i, a) -> shiftP i (constP a)) (listToCanonical xs)
-- toDP (S xs) = sum $ map (\(i, a) -> P ((replicate i 0) ++ [a])) (listToCanonical xs)
toDP (S xs) = P $ go 0 (reverse (listToCanonical xs)) where
    go _ [] = []
    go n ((i, a) : xs) = replicate (i - n) 0 ++ [a] ++ go (i + 1) xs

first :: (a -> a') -> (a, b) -> (a', b)
first f (x, y) = (f x, y)

second :: (b -> b') -> (a, b) -> (a, b')
second f (x, y) = (x, f y)

instance Functor SparsePoly where
    fmap :: (a -> b) -> SparsePoly a -> SparsePoly b
    fmap fun (S xs) = S $ map (second fun) xs

instance Polynomial SparsePoly where
    zeroP :: SparsePoly a
    zeroP = S []

    constP :: (Eq a, Num a) => a -> SparsePoly a
    constP x 
        | x == 0 = S []
        | otherwise = S [(0, x)]

    varP   :: Num a => SparsePoly a                  -- p(x) = x
    varP = S [(1, 1)]

    evalP :: Num a => SparsePoly a -> a -> a        -- value of p(x) at given x
    evalP (S xs) x = go xs where
        go [] = 0
        go (h:xs) = pairSecond h * (x ^ pairFirst h) + go xs

    shiftP :: (Eq a, Num a) => Int -> SparsePoly a -> SparsePoly a -- multiply by x^n
    shiftP n (S xs) = S $ map (first (+n)) xs

    degree :: (Eq a, Num a) => SparsePoly a -> Int -- highest power with nonzero coefficient
    degree (S xs) = case xs of
        [] -> -1
        x : _ -> pairFirst x

instance (Eq a, Num a) => Num (SparsePoly a) where

    abs :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    abs x = undefined

    signum :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    signum x = undefined

    fromInteger :: (Eq a, Num a) => Integer -> SparsePoly a
    fromInteger x
        | x == 0 = S []
        | otherwise = S [(0, fromInteger x)]

    (+) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> SparsePoly a
    (+) x y = S $ listToCanonical (go [] (unS x) (unS y)) where
        go list [] [] = list
        go list [] (y:ys) = go (y:list) [] ys
        go list (x:xs) [] = go (x:list) xs []
        go list (x:xs) (y:ys)
            | pairFirst x == pairFirst y = go ((addSamePower x y):list) xs ys
            | pairFirst x > pairFirst y = go (x:list) xs (y:ys)
            | otherwise = go (y:list) (x:xs) ys

    -- TODO (*)
    -- (*) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> SparsePoly a
    -- (*) x y = S $ sparseToCanonicalAndReverse (go [] (unS x) (unS y)) where
    --     go list [] [] = list
    --     go list [] (y:ys) = go (y:list) [] ys
    --     go list (x:xs) [] = go (x:list) xs []
    --     go list (x:xs) (y:ys) = go ((pairFirst x + pairFirst y, pairSecond x * pairSecond y):list) xs ys

    negate :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    negate x = S $ listToCanonical $ map (\x -> (pairFirst x, negate (pairSecond x))) (unS x)
    -- negate x = S $ reverse (go [] (unS x)) where
    --     go list [] = list
    --     go list (x : xs) = go ((pairFirst x, negate (pairSecond x)) : list) xs

    (-) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> SparsePoly a
    (-) x y = x + negate y


instance (Eq a, Num a) => Eq (SparsePoly a) where
    (==) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> Bool
    p == q = nullP(p-q)

    (/=) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> Bool
    p /= q = not (nullP(p-q))

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP = undefined -- TODO

-- | Division example
-- >>> qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True
