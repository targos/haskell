until' p f = head . filter p . iterate f

position :: (Eq a) => a -> [a] -> Int
position x xs =
  head ([j | (j, y) <- zip [0 ..] xs, y == x] ++ [-1])

x = position 10 [-2 .. 2]

notAllPairs = [(x, y) | x <- [0 ..], y <- [0 ..]]

notAllPairs2 = [(0, y) | y <- [0 ..]]

allPairs = [(x, d - x) | d <- [0 ..], x <- [0 .. d]]

-- Takes two lists in ascending order, and determines whether or not they have an
-- element in common.
disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs [] = True
disjoint [] ys = True
disjoint xs'@(x : xs) ys'@(y : ys)
  | x < y = disjoint xs ys'
  | x == y = False
  | x > y = disjoint xs' ys

quads n =
  [ (a, b, c, d)
  | a <- [1 .. n],
    b <- [a .. n],
    c <- [a + 1 .. n],
    d <- [c .. n],
    a ^ 3 + b ^ 3 == c ^ 3 + d ^ 3
  ]

data List a = Nil | Snoc (List a) a deriving (Show, Eq)

mylist = Snoc (Snoc (Snoc Nil 1) 2) 3

head' :: List a -> a
head' Nil = undefined
head' (Snoc Nil a) = a
head' (Snoc xs _) = head' xs

last' :: List a -> a
last' Nil = undefined
last' (Snoc _ a) = a

toList :: [a] -> List a
toList = convert . reverse
  where
    convert [] = Nil
    convert (x : xs) = Snoc (convert xs) x

fromList :: List a -> [a]
fromList = reverse . convert
  where
    convert Nil = []
    convert (Snoc xs x) = x : (convert xs)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) = drop' (n - 1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([], [])
splitAt' 0 xs = ([], xs)
splitAt' n (x : xs) = (x : a, b)
  where
    (a, b) = splitAt' (n - 1) xs
