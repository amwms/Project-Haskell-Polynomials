{-# LANGUAGE InstanceSigs #-}
module DensePoly() where
import PolyClass
import Representation

instance Functor DensePoly where

instance Polynomial DensePoly where

instance (Eq a, Num a) => Num (DensePoly a) where

    abs :: (Eq a, Num a) => DensePoly a -> DensePoly a
    abs x = undefined

    signum :: (Eq a, Num a) => DensePoly a -> DensePoly a
    signum x = undefined

    -- TODO: fromInteger
    -- fromInteger :: Integer -> DensePoly a

    (+) :: (Eq a, Num a) => DensePoly a -> DensePoly a -> DensePoly a
    (+) x y = P $ reverse $ dropWhile (0 ==) (go [] (unP x) (unP y)) where
        go list (x:xs) (y:ys) = go (x+y:list) xs ys
        go list [] (y:ys) = go (y:list) [] ys
        go list (x:xs) [] = go (x:list) xs []
        go list [] [] = list
        
    -- TODO: (-)
    -- (*) :: (Eq a, Num a) => DensePoly a -> DensePoly a -> DensePoly a

    negate :: (Eq a, Num a) => DensePoly a -> DensePoly a
    negate x = P $ reverse $ dropWhile (0 ==) (go [] (unP x)) where
        go list (x:xs) = go (-x:list) xs
        go list [] = list

-- |
-- >>> x^3 - 1 :: DensePoly Integer 
-- P {unP = [-1,0,0,1]}

-- | Num operations give canonical results:
-- >>> isCanonicalDP (sampleDP - sampleDP)
-- True
    
instance (Eq a, Num a) => Eq (DensePoly a) where    

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
