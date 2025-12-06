-- https://wiki.haskell.org/Sudoku

import Data.List (transpose)

type Matrix a = [Row a]

type Row a = [a]

type Grid = Matrix Digit

type Digit = Char

digits :: [Digit]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

-- Inefficient solve
solve :: Grid -> [Grid]
solve = filter valid . expand . many prune . choices

choices :: Grid -> Matrix [Digit]
choices = map (map choice)

choice d = if blank d then digits else [d]

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [x : ys | x <- xs, ys <- yss]
  where
    yss = cp xss

valid :: Grid -> Bool
valid g =
  all nodups (rows g)
    && all nodups (cols g)
    && all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = all (/= x) xs && nodups xs

rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols = transpose
boxs =
  map ungroup
    . ungroup
    . map cols
    . group
    . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

pruneBy f = f . map pruneRow . f

prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
  where
    fixed = [d | [d] <- row]

remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs

many :: (Eq a) => (a -> a) -> a -> a
many f x = if x == y then x else many f y
  where
    y = f x