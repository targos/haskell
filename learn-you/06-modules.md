# Modules

Notes from <https://learnyouahaskell.github.io/modules.html>.

## Importing modules

* Import before defining functions.
* `import Data.List`.
* In `ghci`: `:m + Data.List`.
* Import only some functions by adding their names: `import Data.List (nub, sort)`.
* Import everything except some functions: `import Data.List hiding (nub)`.
* Import the module as an "object": `import qualified Data.Map as M`.
* Modules from the std lib: <https://downloads.haskell.org/ghc/latest/docs/libraries/>.
* Search modules and functions: <https://hoogle.haskell.org/>.

## `Data.List` module

* `intersperse` takes an element and a list and then puts that element in between each pair of elements in the list.
  * `intersperse '.' "MONKEY"` -> `"M.O.N.K.E.Y"`.
* `intercalate` takes a list and a list of lists. It then inserts that list in between all those lists and then flattens the result.
  * Similar to `Array.join` in JS if applied to list of strings.
  * `intercalate " " ["hey","there","folks"]` -> `"hey there folks"`.
* `transpose` transposes a list of lists.
  * `transpose ["hey","there","folks"]` -> `["htf","eho","yel","rk","es"]`.
  * `map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]` -> `[18,8,6,17]`.
* `foldl'` and `foldl1'` are stricter left folds that aren't too lazy and compute the intermediate values.
* `concat` flattens a list of lists into just a list of elements.
  * `concat ["foo","bar","car"]` -> `"foobarcar"`.
* `concatMap` maps and then concats.
  * `concatMap (replicate 4) [1..3]` -> `[1,1,1,1,2,2,2,2,3,3,3,3]`.
* `and` and `or` take a list of booleans and return a boolean.
  * `and $ map (==4) [4,4,4,3,4]` -> `False`.
* `all` and `any` take a list and a predicate and return a boolean.
  * `any (==4) [2,3,5,6,1,4]` -> `True`.
* `iterate` produces an infinite list from a starting value and a function to apply on the previous value.
  * `take 10 $ iterate (*2) 1` -> `[1,2,4,8,16,32,64,128,256,512]`.
* `splitAt` splits that list at some number of elements. Returns a tuple of lists.
  * `splitAt 4 "abcdef"` -> `("abcd","ef")`.
* `dropWhile` is the inverse of `takeWhile`.
* `span` and `break` separate the list in two using the predicate.
  * `break (==4) [1,2,3,4,5,6,7]` -> `span (/=4) [1,2,3,4,5,6,7]` -> `([1,2,3],[4,5,6,7])`.
* `sort` sorts the list.
* `group` creates a list of lists based on adjacent element equality.
  * `group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]` -> `[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]`.
  * Need to sort the list first if we want groups of unique elements.
* `inits` and `tails` create a growing or decreasing list:
  * `inits "w00t"` -> `["","w","w0","w00","w00t"]`.
* `isInfixOf` returns whether a list includes a sublist anywhere.
  * ``"cat" `isInfixOf` "im a cat burglar"`` -> `True`.
* `isPrefixOf` and `isSuffixOf` search at the beginning and end of the list.
* `elem` and `notElem` check if an element is or isn't inside a list.
* `partition` takes a list and a predicate and returns a pair of lists.
  * ``partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"`` -> `("BOBMORGAN","sidneyeddy")`.
* `find` takes a list and a predicate and returns the first element that satisfies the predicate.
  * But it returns that element wrapped in a `Maybe` value.
* `elemIndex` maybe returns the index of the element.
* `elemIndices` returns a list of indexes.
* `findIndex` and `findIndices` take a predicate.
* The `zip` and `zipWith` functions exist in variants with up to 7 lists (`zip3`, `zipWith4`, etc.).
  * `zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]` -> `[(2,2,5,2),(3,2,5,2),(3,2,3,2)]`.
* `lines` takes a string and returns every line of that string as separate element of a list.
  * `lines "first line\nsecond line\nthird line"` -> `["first line","second line","third line"]`.
* `unlines` is the inverse (like `array.join('\n')` in JS).
* `words` and `unwords` is similar but also handle the space character.
  * `words "hey these           are    the words in this\nsentence"` -> `["hey","these","are","the","words","in","this","sentence"]`.
* `nub` eliminates duplicates from a list.
  * `nub [1,2,3,4,3,2,1,2,3,4,3,2,1]` -> `[1,2,3,4]`.
* `delete` removes the first occurrence of an element.
* `\\` is the difference function (set difference).
  * `[1..10] \\ [2,5,9]` -> `delete 2 . delete 5 . delete 9 $ [1..10]` -> `[1,3,4,6,7,8,10]`.
* `union` appends non-duplicate elements from the second list to the first if they aren't in it.
  * ``[1..7] `union` [5..10]`` -> `[1,2,3,4,5,6,7,8,9,10]`.
* `intersect` only returns elements that exist in both lists.
  * ``[1..7] `intersect` [5..10]`` -> `[5,6,7]`.
* `insert` takes an element and a list of elements that can be sorted and inserts it into the last position where it's still less than or equal to the next element.
  * `insert 4 [3,5,1,2,8,2]` -> `[3,4,5,1,2,8,2]`.
* `length` et al (that take `Int` for historical reasons) have generic versions with `Num` instead.
  * `let xs = [1..6] in sum xs / genericLength xs`.
* `on` from `Data.Function` is useful for `xxxBy` functions.
  * ``groupBy ((==) `on` (> 0)) values``.
  * ``sortBy (compare `on` length) xs``.
  * ``compare `on` length`` is like ``\x y -> length x `compare` length y``.

## `Data.Char` module

