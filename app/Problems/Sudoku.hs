module Sudoku where

import Prelude hiding (and, or, not)
import Prop

data SudokuGrid = SudokuGrid [[Maybe Int]]
  deriving (Eq, Show)

sudoku :: SudokuGrid -> Formula
sudoku (SudokuGrid grid) = and (givenConstraints ++ cellConstraints ++ rowConstraints ++ colConstraints ++ boxConstraints)
  where
    cell i j k = var ("cell_" ++ show i ++ show j ++ show k)

    givenConstraints = concat [case grid !! i !! j of
                               Just v -> [cell i j v]
                               Nothing -> []
                              | i <- [0..3], j <- [0..3]]

    cellConstraints = concat [cellConstraint i j | i <- [0..3 :: Int], j <- [0..3 :: Int]]

    rowConstraints = concat [rowConstraint i v | i <- [0..3 :: Int], v <- [1..4 :: Int]]

    colConstraints = concat [colConstraint j v | j <- [0..3 :: Int], v <- [1..4 :: Int]]

    boxConstraints = concat [boxConstraint bi bj v | bi <- [0..1 :: Int], bj <- [0..1 :: Int], v <- [1..4 :: Int]]

    cellConstraint i j =
      or [cell i j k | k <- [1..4 :: Int]] : [not (and [cell i j k1, cell i j k2]) | k1 <- [1..4 :: Int], k2 <- [k1+1..4 :: Int]]

    rowConstraint i v = [or [cell i j v | j <- [0..3 :: Int]]]

    colConstraint j v = [or [cell i j v | i <- [0..3 :: Int]]]

    boxConstraint bi bj v =
      [or [cell (2*bi + di) (2*bj + dj) v | di <- [0..1], dj <- [0..1]]]

printGrid :: SudokuGrid -> String
printGrid (SudokuGrid grid) = unlines [unwords (map cellStr row) | row <- grid]
  where
    cellStr Nothing = "_"
    cellStr (Just v) = show v

printSolutionGrid :: [Literal] -> String
printSolutionGrid assignment =
  let solution = [[extractValue i j assignment | j <- [0..3 :: Int]] | i <- [0..3 :: Int]]
  in unlines [unwords (map show row) | row <- solution]
  where
    extractValue i j lits =
      let prefix = "cell_" ++ show i ++ show j
      in case [read [last name] :: Int | (Pos (Var name)) <- lits, 
               take (length prefix) name == prefix] of
        [v] -> v
        _ -> 0

