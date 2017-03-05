# CSP

[![Build Status](http://circleci-badges-max.herokuapp.com/img/abarbu/csp-haskell/master?token=91512eefc213cc37bd2c8d8e418debab8cb3efec)](https://circleci.com/gh/abarbu/csp-haskell/tree/master)

This package is available via
[Hackage where its documentation resides](https://hackage.haskell.org/package/csp). It
provides a solver for constraint satisfaction problems by implementing
a `CSP` monad. Currently it only implements arc consistency but other
kinds of constraints will be added.

Below is a Sudoku solver, project Euler problem 96.

```haskell
import Data.List
import Control.Monad.CSP

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ f []     = return ()
mapAllPairsM_ f (_:[]) = return ()
mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l

solveSudoku :: (Enum a, Eq a, Num a) => [[a]] -> [[a]]
solveSudoku puzzle = oneCSPSolution $ do
  dvs <- mapM (mapM (\a -> mkDV $ if a == 0 then [1 .. 9] else [a])) puzzle
  mapM_ assertRowConstraints dvs
  mapM_ assertRowConstraints $ transpose dvs
  sequence_ [assertSquareConstraints dvs x y | x <- [0,3,6], y <- [0,3,6]]
  return dvs
      where assertRowConstraints =  mapAllPairsM_ (constraint2 (/=))
            assertSquareConstraints dvs i j = 
                mapAllPairsM_ (constraint2 (/=)) [(dvs !! x) !! y | x <- [i..i+2], y <- [j..j+2]]

sudoku3 = [[0,0,0,0,0,0,9,0,7],
           [0,0,0,4,2,0,1,8,0],
           [0,0,0,7,0,5,0,2,6],
           [1,0,0,9,0,4,0,0,0],
           [0,5,0,0,0,0,0,4,0],
           [0,0,0,5,0,7,0,0,9],
           [9,2,0,1,0,8,0,0,0],
           [0,3,4,0,5,9,0,0,0],
           [5,0,7,0,0,0,0,0,0]]

solveSudoku sudoku3
```
    
## Future

 - Allow a randomized execution order for CSPs
 - CSPs don't need to use IO internally. ST is enough.
 - Constraint synthesis. Already facilitated by the fact that
   constraints are internally nondeterministic
 - Other constraint types for CSPs, right now only AC is implemented
 - n-ary heterogeneous constraints 
