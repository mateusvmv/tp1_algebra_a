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
    let p = firstPrimeGT (read x)
    let fs = factorizeHuge (p-1)
    let g = generator p
    print $ "Primo: " ++ show p
    print $ "Gerador: " ++ show g
    print $ "Fatoração: " ++ show fs
    t0 <- getCurrentTime
    print $ "Logaritmo: " ++ show (pohligHellman (read a) g p)
    t1 <- getCurrentTime
    print $ "Tempo: " ++ show (diffUTCTime t1 t0)
    print $ "Logaritmo: " ++ show (babyGiantSteps (read a) g p p)
    t2 <- getCurrentTime
    print $ "Tempo: " ++ show (diffUTCTime t2 t1)