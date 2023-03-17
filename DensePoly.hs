{-# LANGUAGE InstanceSigs #-}
module DensePoly() where
import PolyClass
import Representation

instance Functor DensePoly where

instance Polynomial DensePoly where

    zeroP = P []
    constP x = P [x]

    -- varP   :: Num a => p a                  -- p(x) = x

    -- evalP  :: Num a => p a -> a -> a        -- value of p(x) at given x

    shiftP :: (Eq a, Num a) => Int -> DensePoly a -> DensePoly a
    shiftP n (P xs) = P $ replicate n 0 ++ reverse (dropWhile (0 ==) (reverse xs))

    degree :: (Eq a, Num a) => DensePoly a -> Int
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
    (+) x y = P $ reverse 
                    $ dropWhile (0 ==) (go [] (unP x) (unP y)) where
                        go list (x:xs) (y:ys) = go (x+y:list) xs ys
                        go list [] (y:ys) = go (y:list) [] ys
                        go list (x:xs) [] = go (x:list) xs []
                        go list [] [] = list

    -- TODO (*)
    -- (*) :: (Eq a, Num a) => DensePoly a -> DensePoly a -> DensePoly a
    -- (*) x y = go (unP x) (unP y) where
    --     go [] _  = P []
    --     go (x : xs) ys =  P $ reverse $ dropWith (0 ==) reverse (unP (P ((map (*x) ys))) + (P ((0 : (go xs ys)))))

    negate :: (Eq a, Num a) => DensePoly a -> DensePoly a
    negate x = P $ reverse 
                    $ dropWhile (0 ==) (go [] (unP x)) where
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
