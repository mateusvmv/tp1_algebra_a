-- import Data.Numbers.Primes (primes)
import Text.Printf (printf)
import Generator
import Utils
import Primes
import Data.Time
import System.Random (getStdGen, mkStdGen)

main :: IO ()
main = do
    x <- getLine
    let p = firstPrimeGT (read x)
    t0 <- getCurrentTime
    -- print $ randomVals (2, p-1) (mkStdGen 99) !! 10
    print p
    print $ factorizeHuge (p-1)
    print $ generator p
    tf <- getCurrentTime
    print $ diffUTCTime tf t0