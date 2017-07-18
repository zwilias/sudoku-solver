module Sudoku.AltSolver exposing (solve)

import Array.Hamt as Array
import Set exposing (Set)
import Sudoku.Board as Board exposing (Board(..), Position)
import Sudoku.Solution exposing (Solution(..))


emptyPositions : Board -> List ( Position, Set Int )
emptyPositions ((Board board) as b) =
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

                            pos =
                                { row = row, col = col }
                        in
                        Just ( pos, getOptions pos b )
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


getOptions : Position -> Board -> Set Int
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


next : Board -> Maybe ( Position, List Int )
next board =
    emptyPositions board
        |> List.sortBy (Tuple.second >> Set.size)
        |> List.head
        |> Maybe.map (Tuple.mapSecond Set.toList)


solve : Board -> Solution
solve board =
    case next board of
        Nothing ->
            Solved board

        Just ( pos, options ) ->
            solvePosition board pos options


solvePosition : Board -> Position -> List Int -> Solution
solvePosition board pos options =
    case options of
        [] ->
            Failure

        x :: xs ->
            let
                newBoard =
                    Board.set pos x board
            in
            case solve newBoard of
                Solved final ->
                    Solved final

                Failure ->
                    solvePosition board pos xs
