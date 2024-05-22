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
    let fs = factorizePartial (p-1)
    let (g,o) = smallHighOrderElement fs
    let l = pohligHellman (read a) g p
    tf <- getCurrentTime
    print $ "Primo:     " ++ show p
    print $ "Gerador:   " ++ show g
    case o of
        Bounded a b -> do
            print $ "Ordem >=   " ++ show a
            print $ "Ordem <=   " ++ show b
        _ -> return ()
    print $ "Logaritmo: " ++ show l
    print $ diffUTCTime tf t0
