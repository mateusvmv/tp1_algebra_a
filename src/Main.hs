-- import Data.Numbers.Primes (primes)
import Text.Printf (printf)
import Generator
import Utils
import Primes
import DiscreteLog
import Data.Time
import System.Random (getStdGen, mkStdGen)

main :: IO ()
main = do
    x <- getLine
    a <- getLine
    t0 <- getCurrentTime
    let p = firstPrimeGT (read x)
    let fs = factorizeHuge (p-1)
    let g = generator p
    print $ "Primo: " ++ show p
    print $ "Gerador: " ++ show g
    print $ "Logaritmo: " ++ show (pholigHellman (read a) g p)
