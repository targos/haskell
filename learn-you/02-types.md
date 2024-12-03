# Types and Typeclasses

Notes from <https://learnyouahaskell.github.io/types-and-typeclasses.html>.

## Typing functions

```hs
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```

```hs
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z
```

## Some types

* `Int`: 32-bit signed integer.
* `Integer`: arbitrary precision integer.
* `Float`: single-precision floating point.
* `Double`: double-precision floating point.
* `Bool`: boolean.
* `Char`: character.

## Type variables

Lowercase (usually single-letters) means "any type" (similar to generics).

```hs
fst :: (a, b) -> a
```

## Typeclasses
