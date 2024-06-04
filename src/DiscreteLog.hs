module DiscreteLog where

import Utils
import Primes
import GHC.Num.Integer
import Debug.Trace
import Data.List
import Data.Maybe

-- Algorítmo Ingênuo de Logarítmo Discreto
naiveDiscreteLog b a p = fmap fst . find ((==b) . snd) . zip [0..p-1] $ iterate ((`mod`p) . (*a)) 1

-- Calcula o logaritmo discreto de b na base a, modulo p, em um grupo de ordem n, em O(sqrt(n))
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
-- Calcula o logaritmo discreto de b na base a, modulo p, em O(sqrt(p))
babyGiantSteps b a p = babyGiantSteps' b a p p

-- Algoritmo de Pohlig-Hellman para ordem potência de primo - O(e sqrt(p))
pohligPrimePower :: Integer -> Integer -> (Integer, Integer) -> Integer -> Maybe Integer
pohligPrimePower b a (p, e) m = k where
    π i x = binExp x (p^(e-1-i)) m
    solve b i = babyGiantSteps' (π i b) (π 0 a) m p
    it (i, b, k) = (i+1, b',) <$> solve b' (i+1) where
        b' = mod (b * binExp a (m-1-k*(p^i)) m) m
    k = fmap (sum.map (\(i,_,k) -> k*p^i))
        . sequence
        . take (fromInteger e)
        $ iterate (it=<<) ((0,b,) <$> solve b 0)

-- Calcula o logaritmo discreto de a na base g módulo m, com os fatores de m-1, em O(e*sqrt(p)) = O(m^(1/4)), com p sendo o maior fator primo
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

-- Calcula o logaritmo discreto de a na base g módulo m, com os fatores de m-1, em O(e*sqrt(p)) = O(m^(1/4)), com p sendo o maior fator primo
-- Há um limitante, a função não é completa e não aceita entradas muito grandes
discreteLog :: Integer -> Integer -> Factorization -> Maybe Integer
discreteLog a g f
    | m < 2^10 = naiveDiscreteLog a g m
    | m < 2^34 = babyGiantSteps a g m
    | otherwise = case f of
        Full fs -> if maximum fs < 2^34
            then pohligHellman a g fs
            else Nothing
        Partial _ _ -> Nothing
    where m = defactorize f + 1