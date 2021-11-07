{-
An Interpreter for Lu
=====================

In this problem, you will use the state monad to build an *interpreter* and a stepper for
our simple imperative language.

Make sure that you read the [`LuSyntax`](http://www.cis.upenn.edu/~cis552/current/hw/hw06/LuSyntax.html) module that describes
the syntax of Lu before continuing. If you have questions, you
can also consult the [Lu manual](http://www.cis.upenn.edu/~cis552/current/LuManual.html).
-}

module LuStepper where

{-
This assignment uses the State Monad module from the lecture
notes. The relevant definitions are in the file
[`State.hs`](State.hs). Operations such as `get` and `put` are imported as
`S.get` and `S.put`.

Because the Lu language includes primitive tables, we'll also use the finite
 maps from Haskell's containers library (Data.Map).
-}

import Control.Monad (when)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import LuSyntax
import State (State)
import qualified State as S
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import qualified Test.QuickCheck as QC

{-
The Lu Store
------------

One component of the interpreter is a *store* that represents our computer's
memory or heap during the evaluation of Lu prorams. We represent this store
as an associative map from `Name`s to `Tables`:
-}

type Store = Map Name Table

{-
Tables themselves are just finite maps from Lu values to Lu values, with the
invariant that neither keys or values are "nil".
-}

type Table = Map Value Value

{-
This definition for the store makes sense because Lu inteprets global
variables, such as "x", as keys to some top-level table, called "_G".
-}

globalTableName :: Name
globalTableName = "_G"

{-
When the evaluator starts, the initial store will contain this table
for global variables, but the table itself will be empty.
-}

initialStore :: Store
initialStore = Map.singleton globalTableName Map.empty

{-
During computation, the store could be extended to include new mappings for
global variables as well as definitions of new tables.
-}

extendedStore :: Store
extendedStore =
  Map.fromList
    [ ( globalTableName,
        Map.fromList
          [ (StringVal "x", IntVal 3),
            (StringVal "t", TableVal "_t1")
          ]
      ),
      ( "_t1",
        Map.fromList
          [ (StringVal "y", BoolVal True),
            (IntVal 2, TableVal "_t1")
          ]
      )
    ]

{-
Any store can be pretty-printed and displayed concisely.
-}

-- >>> pp initialStore

-- >>> pp extendedStore

{-
Every part of the store can be referred to by the name of a table and a key
for that table.
-}

type Reference = (Name, Value)

{-
For example, if we have a global variable "x", then its reference is
-}

xref :: Reference
xref = ("_G", StringVal "x")

{-
Table fields "t.y" can also be referenced:
-}

yref :: Reference
yref = ("_t1", StringVal "y")

{-
We can use this reference to find out the value of this
variable or update it in an assignment.

Using this general form of reference, we can write functions that looks up
values from the store. (If a table name doesn't exist, of if the key
undefined in that table or if the key is `nil`, then this function should
return the handy `nil` value.)
-}

index :: Reference -> State Store Value
index = undefined

test_index :: Test
test_index =
  "index tests"
    ~: TestList
      [ -- The global variable "x" is unitialized (i.e. not present in the initial store)
        S.evalState (index xref) initialStore ~?= NilVal,
        -- But there is a value for "x" available in the extended store
        S.evalState (index xref) extendedStore ~?= IntVal 3,
        -- If a table is not found in the store, accessing its reference also returns nil.
        S.evalState (index yref) initialStore ~?= NilVal,
        -- We should also be able to access "t[1]" in the extended store
        S.evalState (index yref) extendedStore ~?= BoolVal True,
        -- Updates using the `nil` key are ignored
        S.execState (update ("_t1", NilVal) (IntVal 3)) extendedStore ~?= extendedStore
      ]

-- >>> runTestTT test_index

{-
We can also update values in the store. If the table name doesn't already
exist in the store, this function should do nothing. Otherwise, this
function should modify the table in the store according to the new
association. If the key is nil, there is no change. If the value is nil,
then the association should be removed from the table. Otherwise, the
association should be added to the table.
-}

update :: Reference -> Value -> State Store ()
update = undefined

test_update :: Test
test_update =
  "index tests"
    ~: TestList
      [ -- If we assign to x, then we can find its new value
        S.evalState (update xref (IntVal 4) >> index xref) initialStore ~?= IntVal 4,
        -- If we assign to x, then remove it, we cannot find it anymore
        S.evalState (update xref (IntVal 4) >> update xref NilVal >> index xref) initialStore ~?= NilVal,
        -- If we assign to t.y, then we can find its new value
        S.evalState (update yref (IntVal 5) >> index yref) extendedStore ~?= IntVal 5,
        -- If we assign nil to t.y, then we cannot find it
        S.evalState (update yref NilVal >> index yref) extendedStore ~?= NilVal
      ]

-- >>> runTestTT test_update

{-
Finally, given a list of associations, we can allocate a new table in the
store. To create a fresh name for the new table, we will count the number of
tables already contained in the store and make a name based on that. We've
defined this function already, but you should make sure that you understand
what it does.
-}

allocateTable :: [(Value, Value)] -> State Store Value
allocateTable assocs = do
  store <- S.get
  -- make a fresh name for the new table
  let n = length (Map.keys store)
  let tableName = "_t" ++ show n
  -- make sure we don't have a nil key or value
  let assocs' = filter nonNil assocs
  -- update the store
  S.put (Map.insert tableName (Map.fromList assocs') store)
  return (TableVal tableName)

-- Keep nil out of the table
nonNil :: (Value, Value) -> Bool
nonNil (k, v) = k /= NilVal && v /= NilVal

{-
Expression Evaluator
--------------------

Your next job is to finish `evalE`, an evaluator for expressions.
This function should take as input an expression and return a
store-transformer that yields a `Value`. (See the type of `evalE` below.)

Your expression evaluator should also be *total*.  For any input it should
produce some value. However, we don't have exceptions (or typechecking!), so
if you don't know how to interpret an expression (such as `2 + True` or an
uninitialized variable) your code should return `nil`.

To evaluate variables and access table fields in `evalE` (see below), we'll
use the following helper function, which you must complete.
-}

-- | Convert a variable into a reference into the store.
-- Fails when the var is `t.x` or t[1] and `t` is not defined in the store
-- when the var is `2.y` or `nil[2]` (i.e. not a `TableVal`)
resolveVar :: Var -> State Store (Maybe Reference)
resolveVar = undefined

test_resolveVar :: Test
test_resolveVar =
  "resolveVar"
    ~: TestList
      [ -- we should be able to resolve global variable `x` in the initial store, even though it is not defined
        S.evalState (resolveVar (Name "x")) initialStore ~?= Just ("_G", StringVal "x"),
        -- But in the case of Dot or Proj, the first argument should evaluate to a
        -- TableVal that is defined in the store. If it does not, then resolveVar
        -- should return Nothing.
        S.evalState (resolveVar (Dot (Val NilVal) "x")) initialStore ~?= Nothing,
        S.evalState (resolveVar (Dot (Var (Name "t")) "x")) initialStore ~?= Nothing,
        -- If the table is defined, we should return a reference to it, even when the field is undefined
        S.evalState (resolveVar (Dot (Var (Name "t")) "z")) extendedStore ~?= Just ("_t1", StringVal "z"),
        -- and how we refer to the field shouldn't matter
        S.evalState (resolveVar (Proj (Var (Name "t")) (Val (StringVal "z")))) extendedStore ~?= Just ("_t1", StringVal "z")
      ]

-- >>> runTestTT test_resolveVar

{-
Now implement the rest of `evalE`, referring to the [Lu manual](LuManual.md)
to understand what the unary and binary operators should do. Pay close
attention to equality, ordering and logical `not`. You may wish to define
helper functions as part of your implementation.(As a style check, you
should not use 'S.evalState`, `S.execState`, or `S.runState` any where in
your definition of `evalE` or its helper functions.)
-}

-- | Expression evaluator
evalE :: Expression -> State Store Value
evalE (Var v) = do
  mr <- resolveVar v -- see below
  case mr of
    Just r -> index r
    Nothing -> return NilVal
evalE (Val v) = return v
evalE (Op2 e1 o e2) = evalOp2 o <$> evalE e1 <*> evalE e2
evalE (Op1 _o _e1) = undefined
evalE (TableConst _fs) = undefined

evalOp2 :: Bop -> Value -> Value -> Value
evalOp2 Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalOp2 Divide (IntVal _) (IntVal 0) = NilVal
evalOp2 Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
evalOp2 _ _ _ = NilVal

{-
To test `evalE`, we can write a function that evaluates expressions with a
specified store using the `evalState` operation from the `State` monad
library.
-}

evaluate :: Expression -> Store -> Value
evaluate e = S.evalState (evalE e)

{-
For example, one tricky operator is "not".  Lu interprets both the boolean
false value and nil value as "false" and all other values as true.
-}

-- | Determine whether a value should be interpreted as true or false when
-- used as a condition.
toBool :: Value -> Bool
toBool (BoolVal False) = False
toBool NilVal = False
toBool _ = True

{-
As a result, the `not` operator should invert this behavior and always return either
the true or false value.
-}

test_evaluateNot :: Test
test_evaluateNot =
  "evaluate not"
    ~: TestList
      [ evaluate (Op1 Not (Val NilVal)) initialStore ~?= BoolVal True,
        evaluate (Op1 Not (Val (IntVal 3))) initialStore ~?= BoolVal False
      ]

-- >>> runTestTT test_evaluateNot

{-
Another tricky operator is "len". It must work for both string values and table
values.
-}

test_evaluateLen :: Test
test_evaluateLen =
  "evaluate len"
    ~: TestList
      [ evaluate (Op1 Len (Val (StringVal "552"))) extendedStore ~?= IntVal 3,
        evaluate (Op1 Len (Val (TableVal "_G"))) extendedStore ~?= IntVal 2,
        evaluate (Op1 Len (Val (TableVal "_t1"))) extendedStore ~?= IntVal 2
      ]

-- >>> runTestTT test_evaluateLen

{-
Now would be a good time to add more unit test cases for your expression evaluator.
The two that we have provided above do not test all expression forms that you need to
consider.

You should also make sure that your expression evaluation *always* returns a result,
even for "buggy" code. Your evaluator should never use "error" or trigger
any sort of runtime exception in Haskell.  If the manual doesn't specify
what value to return, you should return "nil".

You can ask quickcheck to test for this property by making sure that every
evaluation returns a value. (The `seq` operator below instructs Haskell to
fully evaluate embedded lazy computations.)
-}

prop_evalE_total :: Expression -> Store -> Bool
prop_evalE_total e s = case evaluate e s of
  NilVal -> True
  IntVal i -> i `seq` True
  BoolVal b -> b `seq` True
  StringVal s -> s `seq` True
  TableVal n -> n `seq` True

{-
Remember that you need to use the terminal for QuickCheck properties.

      > QC.quickCheck prop_evalE_total

(Complete) Statement Evaluator
------------------------------

In this problem, you will need to implement two different evaluators for
statements.  The first one (`evalS`) should evaluate the given statement
completely, if possible. Later, you will need to implement a second evaluator
(`stepS`) that works step-by-step, evaluating only one piece at a time.

Although both evaluators are similar, the first one is a bit simpler. However, this
evaluator could go into an infinite loop if the Lu program does not terminate.
In other words, you should *not* test this evaluator on the program `while true do ; end`.

To help you understand how conditions work, we've provided the cases for `if`
and `while` for you. You'll need to finish the remaining cases.
-}

eval :: Block -> State Store ()
eval (Block ss) = mapM_ evalS ss

-- | Statement evaluator
evalS :: Statement -> State Store ()
evalS (If e s1 s2) = do
  v <- evalE e
  if toBool v then eval s1 else eval s2
evalS w@(While e ss) = do
  v <- evalE e
  when (toBool v) $ do
    eval ss
    evalS w
evalS (Assign _v _e) = undefined -- update global variable or table field v to value of e
evalS (Repeat _b _e) = undefined -- keep evaluating block b until expression e is true
evalS Empty = undefined -- do nothing

{-
In an assignment statement, if the variable refers to an invalid table
(i.e. if `resolveVar` returns `Nothing`, then there is nothing to
do. Otherwise, update the store with the new value at that reference. Like
`if` and `while`, your implementation of `repeat` should use the `toBool`
function above to determine whether the loop should terminate. The
evaluation of `Empty` statement should do nothing.

Don't forget that your evaluator should never call Haskell's "error" or
trigger a runtime exception. However, unlike `evalE`, we cannot use quickcheck
to test totality because of the potential for infinite loops.

To test your evaluator, you can use the following function
-}

exec :: Block -> Store -> Store
exec = S.execState . eval

{-
and the sample program based tests listed below. This command will run them all.
-}

-- >>> runTestTT test_exec

{-
However, if you run into bugs, you'll probably want to write some unit tests at this point.

-}

-------------------------- Test cases for exec -----------------------------

{-
Test cases for the evaluator, using the sample programs defined in the `LuSyntax`
module.
-}

tExecTest :: Test
tExecTest =
  "exec wTest" ~: exec wTest initialStore
    ~?= Map.fromList [(globalTableName, Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 10)])]

tExecFact :: Test
tExecFact =
  "exec wFact" ~: exec wFact initialStore
    ~?= Map.fromList
      [ ( globalTableName,
          Map.fromList
            [ (StringVal "f", IntVal 120),
              (StringVal "n", IntVal 0),
              (StringVal "x", IntVal 1),
              (StringVal "z", IntVal 120)
            ]
        )
      ]

tExecAbs :: Test
tExecAbs =
  "exec wAbs" ~: exec wAbs initialStore
    ~?= Map.fromList
      [ ( globalTableName,
          Map.fromList [(StringVal "x", IntVal 3)]
        )
      ]

tExecTimes :: Test
tExecTimes =
  "exec wTimes" ~: exec wTimes initialStore
    ~?= Map.fromList
      [ ( globalTableName,
          Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 3), (StringVal "z", IntVal 30)]
        )
      ]

tExecTable :: Test
tExecTable =
  "exec wTable" ~: exec wTable initialStore
    ~?= Map.fromList
      [ ( globalTableName,
          Map.fromList
            [ (StringVal "a", TableVal "_t1"),
              (StringVal "k", IntVal 20),
              (StringVal "o1", IntVal 10),
              (StringVal "o2", StringVal "great"),
              (StringVal "o3", IntVal 11)
            ]
        ),
        ("_t1", Map.fromList [(IntVal 20, StringVal "great"), (StringVal "x", IntVal 11)])
      ]

tExecBfs :: Test
tExecBfs = "exec wBfs" ~: TestList [global !? StringVal "found" ~?= Just (BoolVal True)]
  where
    ss = exec wBfs initialStore
    global = case ss !? globalTableName of
      Just g -> g
      Nothing -> Map.empty

test_exec :: Test
test_exec = TestList [tExecTest, tExecFact, tExecAbs, tExecTimes, tExecTable, tExecBfs]

{-
A Stepper for Lu
================

The next part of this problem is to revise the interpreter for blocks and
statements given above so that we can use it in the "stepper" defined in the
`Main` module.

This means that we need to define a function that will *partially* evaluate a
block of statements. The step function below should evaluate the first
statement of the block only, and return the remainder of the block for future
evaluation. Furthermore, in the case of if and while, the step function
should only evaluate the condition and save an appropriate block of
statements for later.
-}

step :: Block -> State Store Block
step _ = undefined

{-
Note that, unlike `evalS`, this step function should *always* terminate, even
if the input program does not. Therefore, we can test this property
using quickcheck.
-}

-- | Make sure that we can step every block in every store
prop_step_total :: Block -> Store -> Bool
prop_step_total b s = case S.runState (step b) s of
  (b', s') -> True

{-
We can also define an operation that evaluates a block for
a specified number of steps.
-}

-- | Evaluate this block for a specified number of steps
boundedStep :: Int -> Block -> State Store Block
boundedStep = undefined

-- | Evaluate this block for a specified nuimber of steps, using the specified store
steps :: Int -> Block -> Store -> (Block, Store)
steps n block = S.runState (boundedStep n block)

{-
Iterating the step function until we do not have any more statements to
execute should have the same behavior as our earlier evaluator. The
`execState` function below should behave exactly the same as `exec` above.
-}

-- | Is this block completely evaluated?
final :: Block -> Bool
final (Block []) = True
final _ = False

-- | Evaluate this block to completion
execStep :: Block -> Store -> Store
execStep = undefined

{-
We can also use quickcheck to test this property, as long as we only compare
programs that terminate within a reasonable number of steps. This property takes a
while to verify because it must discard many of its inputs.
-}

prop_stepExec :: Block -> QC.Property
prop_stepExec b =
  not (final b) QC.==> final b1 QC.==> m1 == m2
  where
    (b1, m1) = S.runState (boundedStep 100 b) initialStore
    m2 = exec b initialStore

{-
Finally, we can also test execStep by evaluating the sample programs.
-}

-- >>> runTestTT test_execStep

tExecStepTest :: Test
tExecStepTest =
  "execStep wTest" ~: execStep wTest initialStore
    ~?= Map.fromList
      [ ( globalTableName,
          Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 10)]
        )
      ]

tExecStepFact :: Test
tExecStepFact =
  "execStep wFact" ~: execStep wFact initialStore
    ~?= Map.fromList
      [ ( globalTableName,
          Map.fromList [(StringVal "f", IntVal 120), (StringVal "n", IntVal 0), (StringVal "x", IntVal 1), (StringVal "z", IntVal 120)]
        )
      ]

tExecStepAbs :: Test
tExecStepAbs =
  "execStep wAbs" ~: execStep wAbs initialStore
    ~?= Map.fromList [(globalTableName, Map.fromList [(StringVal "x", IntVal 3)])]

tExecStepTimes :: Test
tExecStepTimes =
  "execStep wTimes" ~: execStep wTimes initialStore
    ~?= Map.fromList
      [ ( globalTableName,
          Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 3), (StringVal "z", IntVal 30)]
        )
      ]

tExecStepTable :: Test
tExecStepTable =
  "execStep wTable" ~: execStep wTable initialStore
    ~?= Map.fromList
      [ ( globalTableName,
          Map.fromList
            [ (StringVal "a", TableVal "_t1"),
              (StringVal "k", IntVal 20),
              (StringVal "o1", IntVal 10),
              (StringVal "o2", StringVal "great"),
              (StringVal "o3", IntVal 11)
            ]
        ),
        ("_t1", Map.fromList [(IntVal 20, StringVal "great"), (StringVal "x", IntVal 11)])
      ]

tExecStepBfs :: Test
tExecStepBfs =
  "execStep wBfs"
    ~: TestList
      [ global !? StringVal "found" ~?= Just (BoolVal True)
      ]
  where
    ss = execStep wBfs initialStore
    global = case ss !? globalTableName of
      Just g -> g
      Nothing -> Map.empty

test_execStep :: Test
test_execStep = TestList [tExecStepFact, tExecStepAbs, tExecStepTimes, tExecStepAbs, tExecStepTable, tExecStepBfs]

-------------------------- all properties and tests in this module  -----------------------------

test_all :: IO Counts
test_all = runTestTT $ TestList [test_index, test_update, test_resolveVar, test_evaluateNot, test_evaluateLen, test_exec, test_execStep]

-- >>> runTestTT test_all

qc :: IO ()
qc = do
  putStrLn "evalE_total"
  quickCheckN 100 prop_evalE_total
  putStrLn "step_total"
  quickCheckN 100 prop_step_total
  putStrLn "stepExec"
  quickCheckN 100 prop_stepExec
