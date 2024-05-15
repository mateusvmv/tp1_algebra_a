-- import Data.Numbers.Primes (primes)
import Text.Printf (printf)
import Generator
import Utils
import Primes
import Data.Time

main :: IO ()
main = do
    n <- getLine
    a <- getLine

    t0 <- getCurrentTime
    let p = firstPrimeGT (read n)
    let g = generator p
    print p
    print g
    print "logaritmo discreto"
    tf <- getCurrentTime
    print (diffUTCTime tf t0)