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
    let l = discreteLog (read a) g fs
    tf <- getCurrentTime
    putStrLn $ "Primo:     " ++ show p
    putStrLn $ "Gerador:   " ++ show g
    case o of
        Bounded a b -> do
            putStrLn $ "Ordem >=   " ++ show a
            putStrLn $ "Ordem <=   " ++ show b
        _ -> return ()
    putStrLn $ "Logaritmo: " ++ maybe "Inviável" show l
    print $ diffUTCTime tf t0
