{-
State Monad and Applicative Parsing
===================================

This homework provides practice with the state monad and applicative parsing
combinators by developing and implementing a small, imperative programming
language.

This project will require you to first read about the Lu programming language
in the module

[LuSyntax](LuSyntax.html)

and then work in three separate modules

1. [LuStepper](LuStepper.html) (practice with State monad)
2. [LuParser](LuParser.html) (practice with parsing)
3. Main (this file, a "stepper")

You can work on the first two parts in either order, but you'll need to complete
both of them before working on the `stepper` function below.

Throughout the assignment, feel free to
import modules from the
[base](http://hackage.haskell.org/package/base-4.14.1.0) or
[containers](http://hackage.haskell.org/package/containers) packages.
-}

module Main where

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified LuParser as LP
import qualified LuStepper as LS
import LuSyntax
import Text.Read (readMaybe)

{-
Once you have implemented the first two parts of the assignment, you can put
them together in a "stepper" for the Lu language. This interactive program
lets us observe the computation piece-by-piece.

For example, here's an interaction with the factorial function.... Each line beginning
with `Lu>` is a prompt for the tool, allowing the user to type commands such as
`:l` (load a file for stepping), `:n` (step to next statement), `:p` (step to previous statement),
`:e` (evaluate to completion), or evaluate an expression in the current store.
Both `:n` and `:p` take an optional argument for the number of steps.

Lines beginning with `-->` show the current step of the computation (before it
has been executed).

    *Main> stepper
    Lu> :l fact.lu                          -- load a file
    Loaded fact.lu, initializing stepper
    --> n = 5                               -- ready to execute first assignment
    fact.lu> :n
    --> f = 1                               -- "next" ready to executed second assignment
    fact.lu> :n
    --> while n > 0 do                      -- now ready for the while loop
      x = n
      z = f
      while x > 1 do
        f = z + f
        x = x - 1
      end
      n = n - 1
    end
    fact.lu> :n                                -- inside the while loop
    --> x = n
    fact.lu> f                                 -- look up the value of a variable (f)
    1
    --> x = n
    fact.lu> n                                 -- look up another variable (n)
    5
    --> x = n
    fact.lu> x                                 -- we haven't yet assigned to x
    nil
    --> x = n
    fact.lu> :p                                -- we can go backwards!
    --> while n > 0 do
      x = n
      z = f
      while (x > 1) do
        f = z + f
        x = x - 1
      end
      n = n - 1
    end
    fact.lu> :p                                -- another backwards step
    --> f = 1
    fact.lu> f                                 -- now the variable is undefined
    nil
    --> f = 1

Now edit the following stepping function so that it has the behavior shown
above.
-}

data Stepper = Stepper
  { filename :: Maybe String,
    block :: Block,
    store :: LS.Store,
    history :: Maybe Stepper
  }

initialStepper :: Stepper
initialStepper =
  Stepper
    { filename = Nothing,
      block = mempty,
      store = LS.initialStore,
      history = Nothing
    }

-- Fill in `undefined` below
stepper :: IO ()
stepper = go initialStepper
  where
    go :: Stepper -> IO ()
    go ss = do
      prompt ss
      putStr (fromMaybe "Lu" (filename ss) ++ "> ")
      str <- getLine
      case List.uncons (words str) of
        -- load a file for stepping
        Just (":l", [fn]) -> do
          undefined
        -- dump the store
        Just (":d", _) -> do
          putStrLn (pretty (store ss))
          go ss
        -- quit the stepper
        Just (":q", _) -> return ()
        -- run current block to completion
        Just (":r", _) ->
          let s' = LS.exec (block ss) (store ss)
           in go ss {block = mempty, store = s', history = Just ss}
        -- next statement
        Just (":n", strs) -> do
          let numSteps :: Int
              numSteps = case readMaybe (concat strs) of
                Just x -> x
                Nothing -> 1
          undefined
        -- previous statement
        Just (":p", strs) -> do
          let numSteps :: Int
              numSteps = case readMaybe (concat strs) of
                Just x -> x
                Nothing -> 1
          undefined
        -- evaluate an expression in the current state
        _ -> case LP.parseLuExp str of
          Right exp -> do
            let v = LS.evaluate exp (store ss)
            putStrLn (pretty v)
            go ss
          Left _s -> do
            putStrLn "?"
            go ss
    prompt :: Stepper -> IO ()
    prompt Stepper {block = Block []} = return ()
    prompt Stepper {block = Block (s : _)} =
      putStr "--> " >> putStrLn (pretty s)

main :: IO ()
main = do
  putStrLn "*** Testing LuStepper ***"
  LS.test_all
  LS.qc
  putStrLn "*** Testing LuParser ***"
  LP.test_all
  LP.qc
