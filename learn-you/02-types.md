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

* `Int`: signed integer (precision depends on system).
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

Typeclasses are constraints, interfaces for types.

### Some typeclasses

* `Eq`: support for equality testing. Implement `==` and `/=`.
* `Ord`: support for ordering. Can be compared with the `compare` function.
* `Show`: can be presented as strings. Can be converted with the `show` function.
* `Read`: can be parsed from a string: `read "[1,2,3,4]" ++ [5] == [1,2,3,4,5]`.
  We cannot just use `read "42"` alone because the compiler doesn't know which type
  we want in return. Solution: `read "42" :: Int`.
* `Enum`: sequentially ordered types that can be enumerated: `[LT .. GT] == [LT,EQ,GT]`.
* `Bounded`: have an upper and lower bound: `(minBound :: Char) == '\0'`.
  Tuples are part of it if their components are.
* `Num`: numeric. Members can act like numbers.
* `Integral`: whole numbers (`Int`, `Integer`).
  The `fromIntegral` function can transform an integral to a more generic `Num`.
* `Floating`: floating-point (`Float`, `Double`).
