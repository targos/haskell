# Input and Output

Notes from <https://learnyouahaskell.github.io/input-and-output.html>.

## I/O actions

```hs
import Data.Char  
  
main = do  
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words
```

* An I/O action will be performed when we give it a name of `main` and then run our program.
* `do` syntax allows to do multiple I/O actions in a sequence.
* By convention, we don't usually specify a type declaration for `main`.
* `<-` extracts the result from an I/O action.
* Use `let` bindings to bind pure expressions to names.
* Run a program without compiling it with `runhaskell program.hs`.
* `return` creates an I/O action from a pure value.

## I/O functions

* `putStr` print the string to the terminal.
* `putStrLn` print the string with a new line at the end.
* `putChar` print a character.
* `print` takes an instance of `Show` to print its string representation.
* `getChar` reads a character from the input.
* `when` (from `Control.Monad`) takes a boolean and an I/O action and returns it if True, otherwise (`return ()`).
* `sequence` takes a list of I/O actions and returns an I/O action that performs them. `sequence :: [IO a] -> IO [a]`.
  * Useful after using `map` like `sequence (map print [1,2,3,4,5])`.
* `mapM fn` is the same as `sequence . map fn`.
