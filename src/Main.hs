-- import Data.Numbers.Primes (primes)
import Text.Printf (printf)
import Generator
import Utils
import Primes

main :: IO ()
main = do
    n <- getLine
    a <- getLine

    let p = firstPrimeGT (read n)
    let g = generator p
    print p
    print g
    print "logaritmo discreto"
    print "Tempo de processamento??kkkkkkkkk"