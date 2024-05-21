module DiscreteLog where

import Utils
import Primes
import qualified Data.HashMap.Lazy as HM
import GHC.Num.Integer
import Debug.Trace
import Data.List
import Data.Maybe (isNothing)

-- Algoritmo Baby-step Giant-step
babyGiantSteps :: Integer -> Integer -> Integer -> Maybe Integer
babyGiantSteps a g m =
    let
        r = toInteger . ceiling . sqrt . fromIntegral $ m
        powers n = iterate (\x -> x * n `mod` m) 1
        candidatesG = HM.fromListWith (min) $ zip (take (integerToInt r) $ powers g) ([0..(r-1)])
        step = invMod (binExp g r m) m
        test i y = if i < r then
                       case (HM.lookup y candidatesG) of
                         Just j -> traceShow i (Just $ i*r + j)
                         Nothing -> test (i+1) (y*step  `mod` m)
                   else Nothing
    in test 0 a

-- Algoritmo de Pohlig-Hellman para ordem potência de primo - O(e sqrt(p))
pohligPrimePower :: Integer -> Integer -> (Integer, Integer) -> Integer -> Maybe Integer
pohligPrimePower a g (p, e) m =
    let
        gamma = binExp g (p ^ (e-1)) m  -- Has order p
        loop :: Integer -> Integer -> Maybe Integer
        loop k x_bef 
            | k == e = Just x_bef
            | otherwise = case res of 
                Nothing -> Nothing 
                Just res -> loop (k + 1) (x_bef + (res * (p ^ k)) `mod` m)
                where
                    g_inv = invMod g m
                    ak = binExp (binExp g_inv x_bef m * a) (m `div` binExp p (k + 1) m) m
                    res = traceShow (ak, gamma, p) babyGiantSteps ak gamma p
                    
    in loop 0 0


-- Algoritmo de Pohlig-Hellman geral
pohligHellman :: Integer -> Integer -> Integer -> Maybe Integer
pohligHellman a g m
    | any (\(x, val) -> isNothing x) equations = Nothing
    | otherwise = Just $ crt (map (\(Just x, val) -> (x, val)) equations)
    where
        groupOrder = m - 1
        facts = map (\x -> (head x, toInteger . length $ x)) . group . factorize $ groupOrder
        equations = map (\(q, e) ->
            let
                di = q ^ e
                a' = binExp a (groupOrder `div` di) m
                g' = binExp g (groupOrder `div` di) m
                ni = traceShow (a', g', di) (pohligPrimePower a' g' (q, e) m)
            in (ni, di)) facts

-- Calcula o logaritmo discreto de a na base g (gerador!) módulo m
-- discreteLog :: Integer -> Integer -> Integer -> Integer
-- discreteLog a g m
