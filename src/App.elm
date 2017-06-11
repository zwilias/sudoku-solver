port module App exposing (..)

import Array.Hamt as Array
import Json.Encode exposing (Value, string)
import Sudoku.Board as Board exposing (Board(..))
import Sudoku.Solver as Solver exposing (Solution(..))
import Task
import Time


update : Solution -> () -> ( (), Cmd msg )
update solution _ =
    () ! [ emitSolution solution ]


solve : Board -> Cmd Solution
solve board =
    Time.now
        |> Task.map
            (\_ ->
                Solver.solve board
            )
        |> Task.perform identity


main : Program Never () Solution
main =
    Platform.program
        { init = () ! [ solve boardToSolve, emitBoard boardToSolve ]
        , update = update
        , subscriptions = always Sub.none
        }


boardToSolve : Board
boardToSolve =
    [ [ Nothing, Nothing, Just 3, Just 9, Nothing, Nothing, Just 7, Just 6, Nothing ]
    , [ Nothing, Just 4, Nothing, Nothing, Nothing, Just 6, Nothing, Nothing, Just 9 ]
    , [ Just 6, Nothing, Just 7, Nothing, Just 1, Nothing, Nothing, Nothing, Just 4 ]
    , [ Just 2, Nothing, Nothing, Just 6, Just 7, Nothing, Nothing, Just 9, Nothing ]
    , [ Nothing, Nothing, Just 4, Just 3, Nothing, Just 5, Just 6, Nothing, Nothing ]
    , [ Nothing, Just 1, Nothing, Nothing, Just 4, Just 9, Nothing, Nothing, Just 7 ]
    , [ Just 7, Nothing, Nothing, Nothing, Just 9, Nothing, Just 2, Nothing, Just 1 ]
    , [ Just 3, Nothing, Nothing, Just 2, Nothing, Nothing, Nothing, Just 4, Nothing ]
    , [ Nothing, Just 2, Just 9, Nothing, Nothing, Just 8, Just 5, Nothing, Nothing ]
    ]
        |> List.concat
        |> Array.fromList
        |> Board


validBoard : Board
validBoard =
    List.range 0 2
        |> List.concatMap
            (\x ->
                List.range 0 2
                    |> List.map (\i -> (i * 3) % 9 + x)
            )
        |> List.concatMap
            (\x ->
                List.range 0 8
                    |> List.map (\i -> ((x + i) % 9) + 1)
            )
        |> List.map Just
        |> Array.fromList
        |> Board


emitBoard : Board -> Cmd msg
emitBoard board =
    "Attempting to solve:\n\n"
        ++ Board.stringify board
        |> Json.Encode.string
        |> emit


emitSolution : Solution -> Cmd msg
emitSolution solution =
    case solution of
        Solved board ->
            "\n\nSolved!\n\n"
                ++ Board.stringify board
                |> Json.Encode.string
                |> emit

        Failure ->
            Json.Encode.string "\n\nFailed :()" |> emit


port emit : Value -> Cmd msg
