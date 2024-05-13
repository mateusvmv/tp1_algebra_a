module Utils where
import Data.Bits
import Data.List

-- Realiza exponenciacao rapida(binaria) mod p
binExp :: Integer -> Integer -> Integer -> Integer
binExp a b p = 
    let loop acc a' b' = if b' > 0 then 
            (if b' .&. 1 == 1 then 
                loop (acc * a' `mod` p) a' (b'-1) 
            else 
                loop acc (a' * a' `mod` p) (b' `shiftR` 1))
        else 
            acc
    in loop 1 a b


-- Retorna a lista de fatores de um numero
factorize :: Integer -> [Integer]
factorize n
    | n < 2 = []
    | otherwise = loop n 2
    where 
        loop 1 _ = []
        loop n' f
            | f*f > n' = [n']     -- factor > sqrt n 
            | n' `mod` f == 0 = f : loop (n' `div` f) f
            | otherwise = loop n' (f+1)

