primeCandidates = 2:[3,5..]
isPrime n = all (\x -> (n `mod` x) /= 0) (takeWhile (\x -> x*x <= n) primeCandidates)
primes = filter isPrime primeCandidates
nthPrime n = primes !! (n-1)

main :: IO ()
main = print (nthPrime 100_000)
