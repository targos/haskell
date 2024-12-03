# Introduction

Notes from <https://learnyouahaskell.github.io/starting-out.html>

## Interactive mode

```console
> ghci
```

## Operations

* Basic math: `2 + 15`, `49 * 100`, `1892 - 1472`, `5 / 2`, `(50 * 100) - 4999`, `5 * (-3)`.
* Boolean algebra: `True && False`, `False || True`, `not (True && True)`.
* Equality test:
  * `5 == 5`, `1 == 0`, `5 /= 5`, `5 /= 4`, `"hello" == "hello"`.
  * Cannot compare incompatible types. E.g. `"hello" == 1` is an error.

## Functions

### Function call (application)

* In `2 + 15`, `+` is an "infix" function.
* `succ 'a'` returns the "successor" of the parameter. `'b'` in this case.
* `min 10 8` returns the lesser one of the two parameters.
* Function application has the highest precedence:
  * `succ 9 + max 5 4 + 1` is equivalent to `(succ 9) + (max 5 4) + 1`.
  * To get the successor of the product of 9 and 10, you need to write `succ (9 * 10)`.
* A function that takes two parameters can be used as "infix" using backticks: ``92 `div` 10``.
* `odd` returns whether a number is odd: `odd 5 == True`.

### Function definition

* A function is declared like so:
  * Zero parameters: `test = 1`
  * One parameter: `doubleMe x = x + x`.
  * Two parameters: `doubleUs x y = x*2 + y*2`.
* A capital letter is not allowed at the start of a function name.

## If-then-else

```hs
doubleSmallNumber x = if x > 100  
                        then x  
                        else x * 2
```

* It's an expression.
* It returns a value.
* Else is mandatory.

## Lists

```hs
lostNumbers = [4,8,15,16,23,42]
```

* Lists cannot mix types.
* Strings are lists of characters: `"hello"` is the same as `['h','e','l','l','o']`.
* Concatenate lists with the `++` operator: `"hello" ++ " " ++ "world"`.
* Add an element to the beginning of a list with the `:` (cons) operator: `'A':" SMALL CAT"`.
* Get an element with its index (0-based): `"Michael" !! 5 == 'e'`.
* Lists can be compared if their contents can (from the head): `[3,4,2] > [2,4]`.

### Functions on lists

* `head` returns the first element: `head [1,2,3] == 1`.
* `tail` removes the head and returns a list: `tail [1,2,3] == [2,3]`.
* `last` returns the last element: `last [1,2,3] == 3`.
* `init` removes the last element and returns a list: `init [1,2,3] == [1,2]`.
* `length` returns the length: `length [1,2,3] == 3`.
* `null` returns `True` if the list is empty: `null []`.
* `reverse` returns the list in reverse order: `reverse [1,2,3] == [3,2,1]`.
* `take` extracts a number of elements from the beginning of the list: `take 2 [1,2,3] == [1,2]`.
  Can be called on a list smaller than the number of asked elements: `take 4 [1] == [1]`.
* `drop` drops a number of elements and returns the rest: `drop 3 [8,4,2,1,5,6] == [1,5,6]`.
* `maximum` and `minimum` return respectively the biggest and smallest elements: `maximum [[1], [2], [3]] == [3]`.
* `sum` and `product` respectively add and multiply all elements and return the result: `product [2,2,2] == 8`.
* `elem` takes a value and a list and returns whether the list contains the value: ``3 `elem` [1,2,3]``.
* `replicate` takes a number n and a value and returns a list with that value n times: `replicate 3 5 == [5,5,5]`

### Ranges

* `[1..20]` returns a list of all integers from 1 to 20 (inclusive).
* `['c'..'f'] == "cdef"`.
* `[3,6..16] == [3,6,9,12,15]`.
* Avoid using floating point numbers in ranges: `[0.1, 0.3 .. 1] == [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]`.
* No upper bound to make infinite list: `take 12 [2,4..] == [2,4,6,8,10,12,14,16,18,20,22,24]`.
* `cycle` takes a list and cycles it infinitely: `take 10 (cycle [1,2,3]) == [1,2,3,1,2,3,1,2,3,1]`.
* `repeat` takes a value and creates an infinite list with it: `take 3 (repeat 0) == replicate 3 0`.

### List comprehension

* Syntax: `[op | x <- list, pred]`
* Take all elements from the list and apply the operation on the left:
  `[x*2 | x <- [1..10]] == [2,4,6,8,10,12,14,16,18,20]`.
* Take only elements that satisfy the predicate (condition):
  `[x*2 | x <- [1..10], x*2 >= 12] == [12,14,16,18,20]`.
  ``[x | x <- [50..100], x `mod` 7 == 3] == [52,59,66,73,80,87,94]``.
* Multiple predicates are allowed:
  `[x | x <- [10..20], x /= 13, x /= 15, x /= 19] == [10,11,12,14,16,17,18,20]`.
* We can draw from multiple lists (all combinations are tried):
  `length [[x,y,z] | x<-[1..3], y<-[1..3], z<-[1..3]] == 27`.
* We can ignore values:
  `length' xs = sum [1 | _ <- xs]`.

## Tuples

```hs
myTuple = (42, "best")
points = [(1,2),(8,11),(4,5)]
```

## Functions on tuples

* `fst` takes a pair and returns the first component: `fst (1, 2) == 1`.
* `snd` takes a pair and returns the first component: `snd (1, 2) == 2`.
* `zip` takes two lists and creates a list with pairs: `zip [1,2,3] [4,5,6] == [(1,4),(2,5),(3,6)]`.
