import Data.Char (isAlpha, toLower)

type CIN = String

addSum :: CIN -> CIN
addSum cin =
  cin ++ show (n `div` 10) ++ show (n `mod` 10)
  where
    n = sum (map fromDigit cin)

valid :: CIN -> Bool
valid cin = cin == addSum (take 8 cin)

fromDigit :: Char -> Int
fromDigit c = read [c]

palindrome :: IO ()
palindrome = do
  putStrLn "Enter a string:"
  xs <- getLine
  if isPalindrome xs then putStrLn "Yes!" else putStrLn "No!"

isPalindrome :: String -> Bool
isPalindrome str = simplified == reverse simplified
  where
    simplified = map toLower (filter isAlpha str)
