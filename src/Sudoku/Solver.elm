module Sudoku.Solver exposing (Solution(..), solve)

import Array.Hamt as Array
import Set exposing (Set)
import Sudoku.Board as Board exposing (Board(..), Position)


type Solution
    = Failure
    | Solved Board


emptyPositions : Board -> List Position
emptyPositions (Board board) =
    Array.toIndexedList board
        |> List.filterMap
            (\( idx, val ) ->
                case val of
                    Just _ ->
                        Nothing

                    Nothing ->
                        let
                            row =
                                idx // 9

                            col =
                                idx - (9 * row)
                        in
                        Just { row = row, col = col }
            )


verify : Position -> Int -> Board -> Maybe Board
verify pos val oldBoard =
    let
        board =
            Board.set pos val oldBoard
    in
    if
        Board.isValidRow pos.row board
            && Board.isValidColumn pos.col board
            && Board.isValidSquare pos board
    then
        Just board
    else
        Nothing


getOptions : Position -> Board -> List Int
getOptions ({ row, col } as pos) board =
    let
        usedOptions : Set Int
        usedOptions =
            [ Board.row row board
            , Board.column col board
            , Board.square pos board
            ]
                |> List.concat
                |> List.filterMap identity
                |> Set.fromList

        range : Set Int
        range =
            List.range 1 9
                |> Set.fromList
    in
    Set.diff range usedOptions
        |> Set.toList


solve : Board -> Solution
solve board =
    emptyPositions board
        |> solvePositions board


solvePositions : Board -> List Position -> Solution
solvePositions board positions =
    case positions of
        [] ->
            Solved board

        pos :: xs ->
            solvePosition pos (getOptions pos board) xs board


solvePosition : Position -> List Int -> List Position -> Board -> Solution
solvePosition pos options rest board =
    case options of
        [] ->
            Failure

        x :: xs ->
            case verify pos x board of
                Nothing ->
                    solvePosition pos xs rest board

                Just newBoard ->
                    case solvePositions newBoard rest of
                        Solved solution ->
                            Solved solution

                        Failure ->
                            solvePosition pos xs rest newBoard
