module Utils where
import Data.Bits
import System.Random
import Control.Arrow
import Data.Set

-- Fibonacci memoizado
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fib :: Int -> Integer
fib = (fibs !!)

-- Realiza exponenciacao rapida(binaria) mod p
binExp :: Integer -> Integer -> Integer -> Integer
binExp a b p = f' 1 a b where
    f' r a b
      | b == 0 = r
      | odd b = f' (r*a `mod` p) (a^2 `mod` p) (shiftR b 1)
      | otherwise = f' r (a^2 `mod` p) (shiftR b 1)


-- uniqueRandomInts :: (RandomGen g, Random a, Ord a) => (a, a) -> Integer -> g -> [a]
-- uniqueRandomInts range gen = iterate getNext (randomR range gen)
--     where 
--         getNext (x, gen')
--           | x `member` used = getNext (randomR range gen')
--           | otherwise = insert x used
--         used = empty


uniqueRandomInts :: (RandomGen g, Random a, Ord a) => (a, a) -> Int -> g -> ([a], g)
uniqueRandomInts range n = 
    (Prelude.map fst &&& snd . last) . Prelude.take n . removeDuplicatesUsing fst . iterate getNext . randomR range
    where getNext = randomR range . snd

removeDuplicatesUsing::Ord b=>(a->b)->[a]->[a]
removeDuplicatesUsing f theList = [x|(Just x, _, _) <- iterate getNext (Nothing, theList, mempty)]
    where 
      getNext (_, x:xs, used) 
        | f x `member` used = (Nothing, xs, used)
        | otherwise = (Just x, xs, insert (f x) used)