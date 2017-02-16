import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Amb
import Control.Monad.CSP
import Control.Monad
import Data.List

import System.IO.Unsafe

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "constraint1" $
    oneCSPSolution testC0 @?= 2
  , testCase "constraint2 same type" $
    oneCSPSolution testC1 @?= (5,4)
  , testCase "constraint2 different types" $
    oneCSPSolution testC2 @?= ("2",2)
  , testCase "sudoku1" $
    solveSudoku sudoku1 @?= [[4,8,3,9,2,1,6,5,7],[9,6,7,3,4,5,8,2,1],[2,5,1,8,7,6,4,9,3],[5,4,8,1,3,2,9,7,6],[7,2,9,5,6,4,1,3,8],[1,3,6,7,9,8,2,4,5],[3,7,2,6,8,9,5,1,4],[8,1,4,2,5,3,7,6,9],[6,9,5,4,1,7,3,8,2]]
  , testCase "sudoku3" $
    solveSudoku sudoku3 @?= [[4,6,2,8,3,1,9,5,7],[7,9,5,4,2,6,1,8,3],[3,8,1,7,9,5,4,2,6],[1,7,3,9,8,4,2,6,5],[6,5,9,3,1,2,7,4,8],[2,4,8,5,6,7,3,1,9],[9,2,6,1,7,8,5,3,4],[8,3,4,2,5,9,6,7,1],[5,1,7,6,4,3,8,9,2]]
-- Temporarily disabled, sometimes the sudoku file isn't found.
--  , testCase "Euler p96" $
--    length p96 @?= 50
  , testCase "Dinesman's dwellings" $
    dinesmanDwellings @?= [[3,2,4,5,1]]
  ]

testC0 = do
  a <- mkDV [1,2,5]
  constraint1 (==2) a
  return a

testC1 = do
  a <- mkDV [1,2,5]
  b <- mkDV [10,4,7]
  constraint2 (>) a b
  return (a,b)

testC2 = do
  a <- mkDV ["1","2","5"]
  b <- mkDV [3,2,7]
  constraint2 (\a b -> read a == b) a b
  return (a,b)

-- Project Euler problem 96

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

sudoku1 = [[0,0,3,0,2,0,6,0,0],[9,0,0,3,0,5,0,0,1],[0,0,1,8,0,6,4,0,0],[0,0,8,1,0,2,9,0,0],[7,0,0,0,0,0,0,0,8],[0,0,6,7,0,8,2,0,0],[0,0,2,6,0,9,5,0,0],[8,0,0,2,0,3,0,0,9],[0,0,5,0,1,0,3,0,0]]

sudoku3 = [[0,0,0,0,0,0,9,0,7],[0,0,0,4,2,0,1,8,0],[0,0,0,7,0,5,0,2,6],[1,0,0,9,0,4,0,0,0],[0,5,0,0,0,0,0,4,0],[0,0,0,5,0,7,0,0,9],[9,2,0,1,0,8,0,0,0],[0,3,4,0,5,9,0,0,0],[5,0,7,0,0,0,0,0,0]]

p96 :: [(Int, [[Int]])]
p96 = let f = unsafePerformIO $ readFile "sudoku.txt"
      in map (\(g:gs) -> (read $ drop 5 g, solveSudoku $ map (\g -> map (read . (:[])) g) gs))
             $ groupBy (\a b -> not $ isPrefixOf "Grid" b) $ lines f

dinesmanDwellings = allCSPSolutions $ do
  baker <- mkDV [1..5]
  cooper <- mkDV [1..5]
  fletcher <- mkDV [1..5]
  miller <- mkDV [1..5]
  smith <- mkDV [1..5]
  constraint1  (/= 5) baker
  constraint1  (/= 1) cooper
  constraint1  (\x -> x/=1 && x/=5) fletcher
  constraint2 (>) miller cooper
  notAdjacent smith fletcher
  notAdjacent fletcher cooper
  constraint allDistinct  [baker,cooper,fletcher,miller,smith]
  return [baker,cooper,fletcher,miller,smith]

notAdjacent a b = constraint2 (\x y -> abs (x - y) /= 1) a b

allDistinct x = go x []
  where go [] _ = True
        go (x:xs) y
          | x `elem` y = False
          | otherwise = go xs (x:y)
