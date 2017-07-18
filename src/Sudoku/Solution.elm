module Sudoku.Solution exposing (Solution(..))

import Sudoku.Board exposing (Board)


type Solution
    = Failure
    | Solved Board
