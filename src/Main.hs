-- import Data.Numbers.Primes (primes)
import Text.Printf (printf)
import Generator
import Utils
import Primes
import Data.Time
import System.Random (getStdGen, mkStdGen)

main :: IO ()
main = do
    t0 <- getCurrentTime
    -- let (list, _) = uniqueRandomInts (1, 100 :: Integer) 50 (mkStdGen 99)
    x <- getLine
    let p = firstPrimeGT (read x)
    -- print list
    print p
    print $ factorizeHuge (p-1)
    print $ generator p
    tf <- getCurrentTime
    print $ diffUTCTime tf t0