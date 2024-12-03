# Syntax in Functions

Notes from <https://learnyouahaskell.github.io/syntax-in-functions.html>;

## Pattern matching

A function can be declared multiple times with different parameter types:

```hs
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)
```

* Patterns are checked from top to bottom.
* Can be used for recursion.
* If not exhaustive, can result in a runtime error.
* Use `_` to ignore a value:

  ```hs
  second :: (a, b, c) -> b  
  second (_, y, _) = y
  ```

* Pattern matching can be used in list comprehension and lists.
* Patterns allow to keep a reference to the entire thing:

  ```hs
  capital :: String -> String
  capital "" = "Empty string, whoops!"
  capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
  ```

## Guards

A function can have multiple bodies based on conditions:

```hs
densityTell :: (RealFloat a) => a -> String
densityTell density
    | density < 1.2 = "Wow! You're going for a ride in the sky!"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."
```

## Where bindings

Use `where` to avoid computing things twice:

```hs
densityTell :: (RealFloat a) => a -> a -> String  
densityTell mass volume  
    | density < air = "Wow! You're going for a ride in the sky!"  
    | density <= water = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."  
    where density = mass / volume  
          air = 1.2  
          water = 1000.0
```

## Let bindings

Can be used to create temporary variables and use them inside an expression.

## Case expressions

Do do things similar to switch/case, but with pattern matching.
