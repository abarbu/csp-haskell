{-# LANGUAGE TypeFamilies #-}

module Control.Monad.CSP where
import Control.Monad.Amb
import Control.Monad
import Control.Monad.State.Strict
import Data.IORef
import System.IO.Unsafe

data DV r a = DV { dvDomain :: IORef [a], dvConstraints :: IORef [Constraint r] }
type Constraint r = AmbT r IO ()

data DVContainer r = DVContainer { dvcIsBound     :: AmbT r IO Bool,
                                   dvcConstraints :: AmbT r IO (),
                                   dvcABinding    :: AmbT r IO () }

data CSP r x = CSP { unCSP :: IORef [DVContainer r] -> IO x }

csp :: IO x -> CSP r x
csp x = CSP (\_ -> x)

instance Monad (CSP r) where
    CSP x >>= y = CSP (\s -> x s >>= (\(CSP z) -> z s) . y)
    return a = CSP (\_ -> return a)

domain :: DV t t1 -> IO [t1]
domain (DV d _) = readIORef d

demons :: DV r a -> IO [Constraint r]
demons dv = readIORef (dvConstraints dv)

isBound :: DV t t1 -> IO Bool
isBound dv = domain dv >>= return . (== 1) . length

domainSize :: DV t t1 -> IO Int
domainSize dv = domain dv >>= return . length

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

localWriteIORef :: IORef a -> a -> AmbT r IO ()
localWriteIORef ref new = do
  previous <- lift $ readIORef ref
  uponFailure (lift $ writeIORef ref previous)
  lift $ writeIORef ref new

restrictDomain :: DV r a -> ([a] -> IO [a]) -> AmbT r IO ()
restrictDomain dv f = do
  l' <- lift (domain dv >>= f)
  when (null l') fail'
  size <- lift $ domainSize dv
  when (length l' < size) $ do
    localWriteIORef (dvDomain dv) l'
    constraints <- lift $ demons dv
    sequence_ constraints

addConstraint :: DV r1 a -> Constraint r1 -> CSP r ()
addConstraint dv c = csp $ modifyIORef (dvConstraints dv) (c :)

constraint1 :: (a -> Bool) -> DV r1 a -> CSP r ()
constraint1 f dv = addConstraint dv $ restrictDomain dv $ (return . filter f)

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

binding :: DV t b -> IO b
binding d = domain d >>= return . head

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

solveCSP :: CSPResult a => CSP (Result a) a -> Result a
solveCSP (CSP f) =
  (unsafePerformIO $ oneValueT $ do
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
