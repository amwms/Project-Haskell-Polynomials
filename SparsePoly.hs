module SparsePoly(fromDP, toDP, qrP) where
import PolyClass
import Representation
import Data.List (sort, sortOn)

pairFirst :: (a, b) -> a
pairFirst (x, y) = x

pairSecond :: (a, b) -> b
pairSecond (x, y) = y

listToCanonical :: (Eq coef, Num coef) => [(Int, coef)] -> [(Int, coef)]
listToCanonical xs = filter (\x -> snd x /= 0) $ reverse $ go [] (sortOn (negate . fst) xs) where
    go list [] = list
    go [] (x : xs) = go [x] xs
    go (h : list) (x : xs)
        | pairFirst x == pairFirst h = go (addSamePower x h : list) xs
        | otherwise = go (x : h : list) xs

addSamePower :: Num coef => (Int, coef) -> (Int, coef) -> (Int, coef)
addSamePower x y = (pairFirst x, pairSecond x + pairSecond y)

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a

fromDP (P xs) = S $ listToCanonical $ zip [0..] xs

toDP (S xs) = P $ go 0 (reverse (listToCanonical xs)) where
    go _ [] = []
    go n ((i, a) : xs) = replicate (i - n) 0 ++ [a] ++ go (i + 1) xs

first :: (a -> a') -> (a, b) -> (a', b)
first f (x, y) = (f x, y)

second :: (b -> b') -> (a, b) -> (a, b')
second f (x, y) = (x, f y)

instance Functor SparsePoly where
    -- fmap :: (a -> b) -> SparsePoly a -> SparsePoly b
    fmap fun (S xs) = S $ map (second fun) xs

instance Polynomial SparsePoly where
    -- zeroP :: SparsePoly a
    zeroP = S []

    -- constP :: (Eq a, Num a) => a -> SparsePoly a
    constP x
        | x == 0 = S []
        | otherwise = S [(0, x)]

    -- varP   :: Num a => SparsePoly a                 
    varP = S [(1, 1)]

    -- evalP :: Num a => SparsePoly a -> a -> a        
    evalP (S xs) x = go xs where
        go [] = 0
        go (h : xs) = pairSecond h * (x ^ pairFirst h) + go xs

    -- shiftP :: (Eq a, Num a) => Int -> SparsePoly a -> SparsePoly a 
    shiftP n (S xs) = S $ map (first (+n)) $ listToCanonical xs

    -- degree :: (Eq a, Num a) => SparsePoly a -> Int 
    degree (S xs) = case listToCanonical xs of
        [] -> -1
        x : _ -> pairFirst x

instance (Eq a, Num a) => Num (SparsePoly a) where
    -- abs :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    abs x = undefined

    -- signum :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    signum x = undefined

    -- fromInteger :: (Eq a, Num a) => Integer -> SparsePoly a
    fromInteger x = constP (fromInteger x)

    -- (+) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> SparsePoly a
    (+) x y = S $ listToCanonical $ unS x ++ unS y

    -- (*) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> SparsePoly a
    (*) x y = S $ listToCanonical [(exp1 + exp2, coef1 * coef2) | (exp1, coef1) <- unS x, (exp2, coef2) <- unS y]

    -- negate :: (Eq a, Num a) => SparsePoly a -> SparsePoly a
    negate x = S $ listToCanonical $ map (\x -> (pairFirst x, negate (pairSecond x))) (unS x)

    -- (-) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> SparsePoly a
    (-) x y = x + negate y

instance (Eq a, Num a) => Eq (SparsePoly a) where
    -- (==) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> Bool
    p == q = nullP(p-q)

    -- (/=) :: (Eq a, Num a) => SparsePoly a -> SparsePoly a -> Bool
    p /= q = not (nullP(p-q))

-- TODO - check if it always works for non-canonical polynomials
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP x y = go (zeroP, x) where
    go (q, r)
        | nullP r || degree r < degree y = (q, r)
        | otherwise = go (q + res, r - res * y)
        where res = S [(pairFirst (head $ unS r) - pairFirst (head $ unS y), pairSecond (head $ unS r) / pairSecond (head $ unS y))]

-- | Division example
-- >>> qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True
