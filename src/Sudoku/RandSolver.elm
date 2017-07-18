module Sudoku.RandSolver exposing (solve)

import Array.Hamt as Array
import Random.Pcg as Random exposing (Generator)
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


solve : Board -> Generator Solution
solve board =
    case next board of
        Nothing ->
            Random.constant (Solved board)

        Just ( pos, options ) ->
            solvePosition board pos options


solvePosition : Board -> Position -> List Int -> Generator Solution
solvePosition board pos options =
    choose options
        |> Random.andThen
            (\( x, xs ) ->
                case x of
                    Nothing ->
                        Random.constant Failure

                    Just v ->
                        let
                            newBoard =
                                Board.set pos v board
                        in
                        solve newBoard
                            |> Random.andThen
                                (\solution ->
                                    case solution of
                                        Solved _ ->
                                            Random.constant solution

                                        Failure ->
                                            solvePosition board pos xs
                                )
            )


get : Int -> List a -> Maybe a
get index list =
    list
        |> List.drop index
        |> List.head


{-| Sample without replacement: produce a randomly selected element of the
list, and the list with that element omitted. If the list is empty, the
selected element will be `Nothing`.
-}
choose : List a -> Generator ( Maybe a, List a )
choose list =
    if List.isEmpty list then
        Random.constant ( Nothing, list )
    else
        let
            lastIndex =
                List.length list - 1

            front i =
                List.take i list

            back i =
                List.drop (i + 1) list

            gen =
                Random.int 0 lastIndex
        in
        Random.map
            (\index ->
                ( get index list, List.append (front index) (back index) )
            )
            gen
