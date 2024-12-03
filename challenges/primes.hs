primeCandidates = 2:[3,5..]
isPrime n = all (\x -> n `mod` x /= 0) (takeWhile (\x -> x*x <= n) primeCandidates)
primes = [n | n <- primeCandidates, isPrime n]
result = primes !! 99_999

main :: IO ()
main = print result
