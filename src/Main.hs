-- import Data.Numbers.Primes (primes)
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
    let fs = factorizePartial (p-1)
    let (g, o) = smallHighOrderElement fs
    let l = discreteLog (read a) g fs
    t0 <- getCurrentTime
    putStrLn $ "Primo:     " ++ show p
    putStrLn $ "Fatoração: " ++ show fs
    putStrLn $ "Gerador:   " ++ show g
    case o of
        Bounded a b -> do
            putStrLn $ "Ordem >=   " ++ show a
            putStrLn $ "Ordem <=   " ++ show b
        _ -> return ()
    t1 <- getCurrentTime
    putStrLn $ "Tempo:     " ++ show (diffUTCTime t1 t0)
    putStrLn $ "Logaritmo: " ++ maybe "Inviável" show l
    t2 <- getCurrentTime
    putStrLn $ "Tempo:     " ++ show (diffUTCTime t2 t1)
