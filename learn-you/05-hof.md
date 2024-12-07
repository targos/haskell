# Higher order functions

Notes from <https://learnyouahaskell.github.io/higher-order-functions.html>.

## Curried functions

* All functions only have one "real" parameter.
* When we pass less than the expected number of parameters, we get a partially applied function in return.
* Infix functions can be partially applied.

## Maps and filters

* `map` Takes a function and a list and produces a list by applying the function.
* `filter` Takes a predicate and a list and produces a list filtered by the predicate.
* `takeWhile` Takes a predicate and a list and produces a list that stops when the predicate returns False.

## Lambdas

* Lambdas are just anonymous functions that we can pass to higher-order functions.
* Definition: `(\param1 param2 -> result)`.
* Parenthesis is not mandatory but here to avoid full extension to the right.
* You can use pattern matching in lambdas but the pattern must always match.

## Folds

* A fold takes a binary function, a starting value and a list to fold up.
* The binary function is called with the accumulator and the first (or last) element and produces a new accumulator
* This is basically a reducer.
* `foldl` (left fold) folds a list from the left side.
* `foldr` (right fold) folds from the right side, so the accumulator value is passed second to the binary function.
  * Usually `foldr` is better because it works on infinite lists.
* `foldl1` and `foldr1` are similar but take the first element as the starting value. Need at least one element in the list.
* `scanl`, `scanl1` and `scanr`, `scanr1` are like the folds but keep all values and generate a new list.

## Function application

* The `$` function is called function application.
* Function application with a space is left-associative (so `f a b c` is the same as `((f a) b) c)`).
* function application with $ is right-associative.
* Example: `sqrt $ 3 + 4 + 9` vs `sqrt (3 + 4 + 9)`.
* We can map function application over a list of functions: `map ($ 3) [(4+), (10*), (^2), sqrt]`.

## Function composition

* Equivalent to the mathematical `(f°g)(x)=f(g(x))`.
* Done with the `.` function.
* `map (negate . abs) [5,-3,-6,7,-3,2,-19,24]`.
* `map (negate . sum . tail) [[1..5],[3..6],[1..7]]`.
* To use it with functions that take more than one parameter, we must partially apply them first.
* `fn = ceiling . negate . tan . cos . max 50` vs `fn x = ceiling (negate (tan (cos (max 50 x))))`.
