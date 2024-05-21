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


-- Algoritmo de Pohlig-Hellman
pholigHellman :: Integer -> Integer -> Integer -> Maybe Integer
pholigHellman a g m 
    | any (\(x, val) -> isNothing x) equations = Nothing
    | otherwise = Just $ crt (map (\(Just x, val) -> (x, val)) equations)
    where
        groupOrder = m - 1
        facts = map (\x -> (head x, toInteger . length $ x)) . group . factorize $ groupOrder
        equations = map (\(q, e) ->
            let
                di = binExp q e m
                a' = binExp a (groupOrder `div` di) m
                g' = binExp g (groupOrder `div` di) m
                ni = babyGiantSteps a' g' m
            in (ni, di)) facts

-- Calcula o logaritmo discreto de a na base g (gerador!) módulo m
-- discreteLog :: Integer -> Integer -> Integer -> Integer
-- discreteLog a g m
