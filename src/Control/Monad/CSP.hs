{-# LANGUAGE TypeFamilies #-}

module Control.Monad.CSP 
       (
         -- * Overview
         -- $overview

         -- * Building CSPs
         mkDV,
         constraint1,
         constraint2,
         constraint,
         -- * Solving CSPs
         oneCSPSolution,
         allCSPSolutions,
         solveCSP,
         CSPResult(..),
         -- * Low-level internal
         csp,
         domain,
         demons,
         isBound,
         domainSize,
         localWriteIORef,
         binding,
         addConstraint,
         restrictDomain,
         -- * Types
         DV(..),
         DVContainer(..),
         Constraint,
         CSP(..),
       ) where
import Control.Monad.Amb
import Control.Monad
import Control.Monad.State.Strict
import Data.IORef
import System.IO.Unsafe

-- $overview
--
-- This constructs a discrete constraint satisfaction problem (CSP)
-- and then solves it. A discrete CSP consists of a number of
-- variables each having a discrete domain along with a number of
-- constraints between those variables. Solving a CSP searches for
-- assignments to the variables which satisfy those constraints. At
-- the moment the only constraint propagation technique available is
-- arc consistency.
--
--  Here is a simple example which solves Sudoku
-- puzzles, project Euler problem 96.
--
-- @
--import Data.List
--import Control.Monad.CSP
--
--solveSudoku :: (Enum a, Eq a, Num a) => [[a]] -> [[a]]
--solveSudoku puzzle = oneCSPSolution $ do
--  dvs \<- mapM (mapM (\\a -> mkDV $ if a == 0 then [1 .. 9] else [a])) puzzle
--  mapM_ assertRowConstraints dvs
--  mapM_ assertRowConstraints $ transpose dvs
--  sequence_ [assertSquareConstraints dvs x y | x <- [0,3,6], y <- [0,3,6]]
--  return dvs
--      where assertRowConstraints =  mapAllPairsM_ (constraint2 (/=))
--            assertSquareConstraints dvs i j = 
--                mapAllPairsM_ (constraint2 (/=)) [(dvs !! x) !! y | x <- [i..i+2], y <- [j..j+2]]
--
-- mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
-- mapAllPairsM_ f []     = return ()
-- mapAllPairsM_ f (_:[]) = return ()
-- mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l
--
--sudoku3 = [[0,0,0,0,0,0,9,0,7],
--           [0,0,0,4,2,0,1,8,0],
--           [0,0,0,7,0,5,0,2,6],
--           [1,0,0,9,0,4,0,0,0],
--           [0,5,0,0,0,0,0,4,0],
--           [0,0,0,5,0,7,0,0,9],
--           [9,2,0,1,0,8,0,0,0],
--           [0,3,4,0,5,9,0,0,0],
--           [5,0,7,0,0,0,0,0,0]]
-- @
--
-- >>> solveSudoku sudoku3
-- [[4,6,2,8,3,1,9,5,7],[7,9,5,4,2,6,1,8,3],[3,8,1,7,9,5,4,2,6],[1,7,3,9,8,4,2,6,5],[6,5,9,3,1,2,7,4,8],[2,4,8,5,6,7,3,1,9],[9,2,6,1,7,8,5,3,4],[8,3,4,2,5,9,6,7,1],[5,1,7,6,4,3,8,9,2]]


data DV r a = DV { dvDomain :: IORef [a], dvConstraints :: IORef [Constraint r] }
type Constraint r = AmbT r IO ()

data DVContainer r = DVContainer { dvcIsBound     :: AmbT r IO Bool,
                                   dvcConstraints :: AmbT r IO (),
                                   dvcABinding    :: AmbT r IO () }

data CSP r x = CSP { unCSP :: IORef [DVContainer r] -> IO x }

-- | Lift an IO computation into the CSP monad. CSPs are only in IO
-- temporarily.
csp :: IO x -> CSP r x
csp x = CSP (\_ -> x)

instance Functor (CSP r) where
    fmap = liftM
 
instance Applicative (CSP r) where
    pure  = return
    (<*>) = ap

instance Monad (CSP r) where
    CSP x >>= y = CSP (\s -> x s >>= (\(CSP z) -> z s) . y)
    return a = CSP (\_ -> return a)

-- | Extract the current domain of a variable.
domain :: DV t t1 -> IO [t1]
domain (DV d _) = readIORef d

-- | Extract the current constraints of a variable.
demons :: DV r a -> IO [Constraint r]
demons dv = readIORef (dvConstraints dv)

-- | Is the variable currently bound?
isBound :: DV t t1 -> IO Bool
isBound dv = domain dv >>= return . (== 1) . length

-- | Compute the size of the current domain of variable.
domainSize :: DV t t1 -> IO Int
domainSize dv = domain dv >>= return . length

-- | Create a variable with the given domain
mkDV :: [a] -> CSP r (DV r a)
mkDV xs = do
  d <- csp $ newIORef xs
  c <- csp $ newIORef []
  let dv = DV d c
  CSP (\x -> modifyIORef x $ ((DVContainer (lift $ isBound dv)
                               (lift (demons dv) >>= sequence_)
                               (do
                                   d' <- lift $ readIORef d
                                   e  <- aMemberOf d'
                                   restrictDomain dv (\_ -> return [e])))
                              :))
  return dv

-- | This performs a side-effect, writing to the given IORef but
-- records this in the nondeterministic computation so that it can be
-- undone when backtracking.
localWriteIORef :: IORef a -> a -> AmbT r IO ()
localWriteIORef ref new = do
  previous <- lift $ readIORef ref
  uponFailure (lift $ writeIORef ref previous)
  lift $ writeIORef ref new

-- | The low-level function out of which constraints are
-- constructed. It modifies the domain of a variable.
restrictDomain :: DV r a -> ([a] -> IO [a]) -> AmbT r IO ()
restrictDomain dv f = do
  l' <- lift (domain dv >>= f)
  when (null l') empty
  size <- lift $ domainSize dv
  when (length l' < size) $ do
    localWriteIORef (dvDomain dv) l'
    constraints <- lift $ demons dv
    sequence_ constraints

-- | Add a constraint to the given variable.
addConstraint :: DV r1 a -> Constraint r1 -> CSP r ()
addConstraint dv c = csp $ modifyIORef (dvConstraints dv) (c :)

-- | Assert a unary constraint.
constraint1 :: (a -> Bool) -> DV r1 a -> CSP r ()
constraint1 f dv = addConstraint dv $ restrictDomain dv $ (return . filter f)

-- | Assert a binary constraint with arc consistency.
constraint2 :: (a -> t1 -> Bool) -> DV t a -> DV t t1 -> CSP r ()
constraint2 f x y = do
  addConstraint x $
    restrictDomain y
      (\yd -> do
          xd <- (domain x)
          return $ filter (\ye -> any (\xe -> f xe ye) xd) yd)
  addConstraint y $
    restrictDomain x
      (\xd -> do
          yd <- (domain y)
          return $ filter (\xe -> any (\ye -> f xe ye) yd) xd)

-- | Assert an n-ary constraint with arc consistency. One day this
-- will allow for a heterogeneous list of variables, but at the moment
-- they must all be of the same type.
constraint :: ([a] -> Bool) -> [DV r1 a] -> CSP r ()
constraint f dvl =
  mapM_ (\(dv1, k) ->
          addConstraint dv1 $
          (mapM_ (\(dv2, i) -> do
                        unless (i == k) $ 
                          restrictDomain dv2
                             (\d2 -> do
                                 ddvl <- mapM domain dvl
                                 return $ filter (\d2e -> 
                                                   let loop []     es _ = f (reverse es)
                                                       loop (d:ds) es j | i == j = loop ds (d2e:es) (j + 1)
                                                                        | otherwise = any (\e -> loop ds (e : es) (j + 1)) d
                                                   in loop ddvl [] 0) d2))
                 $ zip dvl ([1..] :: [Int])))
      $ zip dvl ([1..] :: [Int])

-- | Retrieve the current binding of a variable.
binding :: DV t b -> IO b
binding d = domain d >>= return . head

-- | This extracts results from a CSP.
class CSPResult a where
    type Result a
    result :: a -> IO (Result a)
instance CSPResult (DV r a) where
    type Result (DV r a) = a
    result = binding
instance (CSPResult a, CSPResult b) => CSPResult (a,b) where
    type Result (a,b) = (Result a, Result b)
    result (a,b) = do
      a' <- result a
      b' <- result b
      return (a', b')
instance (CSPResult a) => CSPResult [a] where
    type Result [a] = [Result a]
    result = mapM result

-- | Solve the given CSP. The CSP solver is a nondeterministic
-- function in IO and this is the generic interface which specifies
-- how the nondeterministic computation should be carried out.
solveCSP :: CSPResult a1 => (AmbT r IO (Result a1) -> IO a) -> CSP r a1 -> a
solveCSP runAmb (CSP f) =
  (unsafePerformIO $ runAmb $ do
      dvcs  <- lift $ newIORef []
      r     <- lift $ f dvcs
      dvcs' <- lift $ readIORef dvcs
      -- One round of applying all constraints
      mapM_ dvcConstraints dvcs'
      let loop [] = return ()
          loop (d:ds) = do
            dvcABinding d
            filterM (liftM not . dvcIsBound) ds >>= loop
        in filterM (liftM not . dvcIsBound) dvcs' >>= loop
      lift $ result r >>= return)

-- | Return a single solution to the CSP. 'solveCSP' running with 'oneValueT'
oneCSPSolution :: CSPResult a1 => CSP (Result a1) a1 -> Result a1
oneCSPSolution = solveCSP oneValueT

-- | Return all solutions to the CSP. 'solveCSP' running with
-- 'allValuesT'
allCSPSolutions :: CSPResult a1 => CSP (Result a1) a1 -> [Result a1]
allCSPSolutions = solveCSP allValuesT
