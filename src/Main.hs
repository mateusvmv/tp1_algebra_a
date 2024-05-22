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
    t0 <- getCurrentTime
    let p = firstPrimeGT (read x)
    let fs = factorizePartial (p-1)
    let (g, o) = smallHighOrderElement fs
    -- print $ randomVals (2, p-1) (mkStdGen 99) !! 10
    print p
    print fs
    print g
    case o of
        Bounded _ _ -> print o
        _ -> return ()
    tf <- getCurrentTime
    print $ diffUTCTime tf t0