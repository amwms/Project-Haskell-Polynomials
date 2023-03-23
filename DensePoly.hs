{-# LANGUAGE InstanceSigs #-}
module DensePoly() where
import PolyClass
import Representation

toCanonical :: (Eq a, Num a) => [a] -> [a]
toCanonical xs = reverse $ dropWhile (0 ==) (reverse xs)

reverseToCanonical :: (Eq a, Num a) => [a] -> [a]
reverseToCanonical xs = reverse $ dropWhile (0 ==) xs

instance Functor DensePoly where
    fmap :: (a -> b) -> DensePoly a -> DensePoly b
    fmap fun (P xs) = P $ map fun xs

instance Polynomial DensePoly where

    zeroP :: DensePoly a
    zeroP = P []

    constP :: (Eq a, Num a) => a -> DensePoly a
    constP x = P [x]

    varP   :: Num a => DensePoly a                  -- p(x) = x
    varP = P [0, 1]

    evalP  :: Num a => DensePoly a -> a -> a        -- value of p(x) at given x
    evalP (P xs) x = go xs 0 where
        go [] _ = 0
        go (h : xs) n = h * (x ^ n) + go xs (n + 1)

    shiftP :: (Eq a, Num a) => Int -> DensePoly a -> DensePoly a -- multiply by x^n
    shiftP n (P xs) = P $ replicate n 0 ++ toCanonical xs

    degree :: (Eq a, Num a) => DensePoly a -> Int -- highest power with nonzero coefficient
    degree (P xs) = length (dropWhile (0 ==) (reverse xs)) - 1

instance (Eq a, Num a) => Num (DensePoly a) where

    abs :: (Eq a, Num a) => DensePoly a -> DensePoly a
    abs x = undefined

    signum :: (Eq a, Num a) => DensePoly a -> DensePoly a
    signum x = undefined

    fromInteger :: Integer -> DensePoly a
    fromInteger x
        | x == 0 = P []
        | otherwise = P [fromInteger x]

    (+) :: (Eq a, Num a) => DensePoly a -> DensePoly a -> DensePoly a
    (+) x y = P $ reverseToCanonical (go [] (unP x) (unP y)) where
        go list [] [] = list
        go list (x : xs) [] = go (x : list) xs []
        go list [] (y : ys) = go (y : list) [] ys
        go list (x : xs) (y : ys) = go (x + y : list) xs ys

    -- -- TODO (*)
    (*) :: (Eq a, Num a) => DensePoly a -> DensePoly a -> DensePoly a
    (*) x y = go (unP x) (unP y) where
        go [] _  = P []
        go (x:xs) ys =  P $ toCanonical (unP $ (P ((map (*x) ys))) + (P ((0 : unP (go xs ys)))))


    negate :: (Eq a, Num a) => DensePoly a -> DensePoly a
    negate x = P $ toCanonical $ map (* (-1)) (unP x)
    -- negate x = P $ reverseToCanonical (go [] (unP x)) where
    --     go list [] = list
    --     go list (x : xs) = go (-x : list) xs

-- |
-- >>> x^3 - 1 :: DensePoly Integer 
-- P {unP = [-1,0,0,1]}

-- | Num operations give canonical results:
-- >>> isCanonicalDP (sampleDP - sampleDP)
-- True

instance (Eq a, Num a) => Eq (DensePoly a) where
    (==) :: DensePoly a -> DensePoly a -> Bool
    (==) x y = degree x == degree y && dropWhile (0 ==) (reverse (unP x)) ==  dropWhile (0 ==) (reverse (unP y))

    (/=) :: DensePoly a -> DensePoly a -> Bool
    (/=) x y = degree x /= degree y || dropWhile (0 ==) (reverse (unP x)) /= dropWhile (0 ==) (reverse (unP y))

-- |
-- >>>  P [1,2] == P [1,2]
-- True

-- |
-- >>> fromInteger 0 == (zeroP :: DensePoly Int)
-- True

-- |
-- >>>  P [0,1] == P [1,0]
-- False

-- | Degree examples
-- >>> degree (zeroP :: DensePoly Int)
-- -1
-- >>> degree (constP 1 :: DensePoly Int)
-- 0
