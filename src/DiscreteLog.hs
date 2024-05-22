module DiscreteLog where

import Utils
import Primes
import qualified Data.HashMap.Lazy as HM
import GHC.Num.Integer
import Debug.Trace
import Data.List
import Data.Maybe (isNothing)

-- Algoritmo Baby-step Giant-step
babyGiantSteps' :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
babyGiantSteps' b a p n = f xs ys where
    r = toInteger . ceiling . sqrt . fromIntegral $ n
    s = invMod (binExp a r p) p
    xs = sortOn fst $ zip (iterate ((`mod`p).(*a)) 1) [0..r-1]
    ys = sortOn fst $ zip (iterate ((`mod`p).(*s)) b) [0..r-1]
    f [] _ = Nothing
    f _ [] = Nothing
    f xxs@((x,i):xs) yys@((y,j):ys)
        | x>y = f xxs ys
        | x<y = f xs yys
        | otherwise = Just $ j*r + i
babyGiantSteps b a p = babyGiantSteps' b a p p

-- Algoritmo de Pohlig-Hellman para ordem potência de primo - O(e sqrt(p))
pohligPrimePower :: Integer -> Integer -> (Integer, Integer) -> Integer -> Maybe Integer
pohligPrimePower a g (p, e) m =
    let
        n = binExp p e m
        gamma = binExp g (binExp p (e-1) m) m
        loop :: Integer -> Integer -> Maybe Integer
        loop k x_bef
            | k == e = Just x_bef
            | otherwise = case res of 
                Nothing -> Nothing 
                Just res' -> loop (k + 1) (x_bef + (res' * p_k) `mod` n)
                where
                    p_k = p ^ k
                    g_inv = binExp g (m-1-x_bef) m 
                    ak = binExp (g_inv * a) (n `div` (p ^ (k + 1))) m
                    res = babyGiantSteps' ak gamma m p
                    
    in loop 0 0


-- Algoritmo de Pohlig-Hellman geral
pohligHellman :: Integer -> Integer -> [Integer] -> Maybe Integer
pohligHellman a g f
    | any (\(x, val) -> isNothing x) equations = Nothing
    | otherwise = Just $ crt (map (\(Just x, val) -> (x, val)) equations)
    where
        m = product f + 1
        groupOrder = m - 1
        facts = map (\x -> (head x, toInteger . length $ x)) . group . factorize $ groupOrder
        equations = map (\(q, e) ->
            let
                di = binExp q e m
                a' = binExp a (groupOrder `div` di) m
                g' = binExp g (groupOrder `div` di) m
                ni = pohligPrimePower a' g' (q, e) m
            in (ni, di)) facts

-- Calcula o logaritmo discreto de a na base g (gerador!) módulo m
-- discreteLog :: Integer -> Integer -> Integer -> Integer
-- discreteLog a g m
discreteLog :: Integer -> Integer -> Factorization -> Maybe Integer
discreteLog a g f
    | m < 2^20 = babyGiantSteps a g m
    | otherwise = case f of
        Full fs -> if maximum fs < 2^30
            then pohligHellman a g fs
            else Nothing
        Partial _ _ -> Nothing
    where m = defactorize f + 1