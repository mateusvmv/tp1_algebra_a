import Data.Numbers.Primes (primes)
import Text.Printf (printf)
import Generator
import Utils

main :: IO ()
main = do
    n <- getLine
    x <- getLine
    let l = takeWhile(< (read n :: Integer)) primes
    mapM_ (\x -> printf "%d, %d\n" x (generator x)) l

    print (factorize (read x))