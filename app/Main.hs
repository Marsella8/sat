module Main where

import Prop
import Int
import Sudoku
import Conversions
import DPLL

solveInt4 :: IO ()
solveInt4 = do
  let wff = constraints 4
  let cnfWff = wffToCNF wff
  putStrLn "Checking if addition is commutative for 4-bit integers..."
  putStrLn $ "Variables: " ++ show (length (vars cnfWff))
  putStrLn "Solving with DPLL..."
  case dpllAssigned (cnfWff, []) of
    Just assigned -> do
      putStrLn "SAT: Found a counterexample to commutativity!"
      putStrLn "Solution:"
      print assigned
    Nothing -> putStrLn "Addition is commutative."
  putStrLn ""

solveSudoku :: IO ()
solveSudoku = do
  let gridData = [ [Nothing, Nothing, Nothing, Just 4]
                 , [Nothing, Nothing, Nothing, Nothing]
                 , [Just 2, Nothing, Nothing, Just 3]
                 , [Just 4, Nothing, Just 1, Just 2]
                 ]
  let grid = SudokuGrid gridData
  putStrLn "Solving 4x4 Sudoku puzzle:"
  putStr (printGrid grid)
  putStrLn ""
  let sudokuConstraints = sudoku grid
  let cnfConstraints = wffToCNF sudokuConstraints
  putStrLn $ "Variables: " ++ show (length (vars cnfConstraints))
  case dpllAssigned (cnfConstraints, []) of
    Just assigned -> do
      putStrLn "SAT: Found a solution!"
      putStr (printSolutionGrid assigned)
    Nothing -> putStrLn "UNSAT: No solution exists"

main :: IO ()
main = do
  solveInt4
  solveSudoku
