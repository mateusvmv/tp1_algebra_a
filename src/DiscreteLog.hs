module DiscreteLog where

import Utils
import qualified Data.HashMap.Lazy as HM
import GHC.Num.Integer
import Debug.Trace

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

-- Calcula o logaritmo discreto de a na base g (gerador!) módulo m
-- discreteLog :: Integer -> Integer -> Integer -> Integer
-- discreteLog a g m
