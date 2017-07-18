port module App exposing (..)

import Array.Hamt as Array
import Json.Encode exposing (Value, string)
import Random.Pcg as Random
import Sudoku.AltSolver as AltSolver
import Sudoku.Board as Board exposing (Board(..))
import Sudoku.RandSolver as RandSolver
import Sudoku.Solution exposing (Solution(..))
import Sudoku.Solver as Solver
import Task
import Time


type Msg
    = Solve String Solution


update : Msg -> () -> ( (), Cmd msg )
update (Solve solver solution) _ =
    () ! [ emitSolution solver solution ]


solveWithRand : Board -> Cmd Msg
solveWithRand board =
    Random.generate (Solve "randomized solver") (RandSolver.solve board)


solveWithNaive : Board -> Cmd Msg
solveWithNaive board =
    Time.now
        |> Task.map
            (\_ ->
                Solver.solve board
            )
        |> Task.perform (Solve "naive")


solveWithAlt : Board -> Cmd Msg
solveWithAlt board =
    Time.now
        |> Task.map
            (\_ ->
                AltSolver.solve board
            )
        |> Task.perform (Solve "alt")


solve : Board -> Cmd Msg
solve board =
    solveWithAlt board


main : Program Never () Msg
main =
    let
        board =
            datBoard
    in
    Platform.program
        { init = () ! [ solve board, emitBoard board ]
        , update = update
        , subscriptions = always Sub.none
        }


emptyBoard : Board
emptyBoard =
    Array.initialize 81 (always Nothing) |> Board


datBoard : Board
datBoard =
    [ [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Just 8, Nothing, Nothing, Just 6, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Just 3, Just 2, Just 9, Just 8, Just 6 ]
    , [ Nothing, Nothing, Just 7, Nothing, Just 8, Nothing, Just 2, Nothing, Just 1 ]
    , [ Nothing, Nothing, Nothing, Nothing, Just 2, Nothing, Just 3, Just 4, Nothing ]
    , [ Just 9, Nothing, Nothing, Just 7, Nothing, Nothing, Just 6, Nothing, Nothing ]
    , [ Just 1, Nothing, Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing ]
    , [ Just 5, Just 9, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Just 8, Nothing, Just 1, Just 5, Nothing, Nothing, Nothing, Nothing ]
    ]
        |> List.concat
        |> Array.fromList
        |> Board


hardToSolve : Board
hardToSolve =
    [ List.repeat 9 Nothing
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Just 3, Nothing, Just 8, Just 5 ]
    , [ Nothing, Nothing, Just 1, Nothing, Just 2, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Just 5, Nothing, Just 7, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Just 4, Nothing, Nothing, Nothing, Just 1, Nothing, Nothing ]
    , [ Nothing, Just 9, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Just 5, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 7, Just 3 ]
    , [ Nothing, Nothing, Just 2, Nothing, Just 1, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Just 4, Nothing, Nothing, Nothing, Just 9 ]
    ]
        |> List.concat
        |> Array.fromList
        |> Board


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


emitSolution : String -> Solution -> Cmd msg
emitSolution solver solution =
    case solution of
        Solved board ->
            "\n\nSolved with "
                ++ solver
                ++ "!\n\n"
                ++ Board.stringify board
                |> Json.Encode.string
                |> emit

        Failure ->
            Json.Encode.string "\n\nFailed :(" |> emit


port emit : Value -> Cmd msg
