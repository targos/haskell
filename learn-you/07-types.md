# Making Our Own Types and Typeclasses

Notes from <https://learnyouahaskell.github.io/making-our-own-types-and-typeclasses.html>.

## Algebraic data types

* Define a data type with the `data` keyword.
  * `data Bool = False | True`.
  * `data Shape = Circle Float Float Float | Rectangle Float Float Float Float`.
* Add `deriving (Show)` to the type so it can be shown.

## Record syntax

```hs
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)
```

* Allows to give names to the fields.
* Automatically creates functions to get the value for each field.

## Type parameters

* `data Maybe a = Nothing | Just a` -> `a` is the type parameter.
* never add typeclass constraints in data declarations (`data (Ord k) => Map k v = ...`).

## Derived instances

* `data X = X {...} deriving (Eq)`.
  * Will check if the constructor is the same and then compare every field with `==`.

## Type synonyms

* `type String = [Char]`.
* Similar type TypeScript.
* `type AssocList k v = [(k,v)]` -> parametrized.
* `type IntMap = Map Int` -> partial.
* Type synonyms can only be used in type portions of the language.
* `Either a b` type: when there is an error, it's in type `Left a`, result in type `Right b`.

## Recursive data structures

