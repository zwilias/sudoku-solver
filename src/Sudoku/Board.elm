module Sudoku.Board
    exposing
        ( Board(..)
        , Position
        , column
        , isValidColumn
        , isValidRow
        , isValidSquare
        , row
        , set
        , square
        , stringify
        )

import Array.Hamt as Array exposing (Array)
import List.Extra as List
import Set


type Board
    = Board (Array (Maybe Int))


type alias Position =
    { row : Int, col : Int }


isValidSquare : Position -> Board -> Bool
isValidSquare { col, row } board =
    let
        lowerCol =
            (col // 3) * 3

        lowerRow =
            (row // 3) * 3
    in
    List.range lowerCol (lowerCol + 2)
        |> List.concatMap
            (\col ->
                List.range lowerRow (lowerRow + 2)
                    |> List.map (\row -> { row = row, col = col })
                    |> List.filterMap (flip get board)
            )
        |> isValidRange


isValidRow : Int -> Board -> Bool
isValidRow row board =
    List.range 0 8
        |> List.map (\col -> { row = row, col = col })
        |> List.filterMap (flip get board)
        |> isValidRange


isValidColumn : Int -> Board -> Bool
isValidColumn col board =
    List.range 0 8
        |> List.map (\row -> { row = row, col = col })
        |> List.filterMap (flip get board)
        |> isValidRange


isValidRange : List Int -> Bool
isValidRange xs =
    let
        listLength =
            List.length xs
    in
    if listLength == 0 then
        False
    else
        Set.fromList xs
            |> Set.size
            |> (==) listLength


toOffset : Position -> Int
toOffset { row, col } =
    col + 9 * row


row : Int -> Board -> List (Maybe Int)
row row board =
    List.range 0 8
        |> List.map (\col -> { row = row, col = col })
        |> List.map (flip get board)


column : Int -> Board -> List (Maybe Int)
column col board =
    List.range 0 8
        |> List.map (\row -> { row = row, col = col })
        |> List.map (flip get board)


square : Position -> Board -> List (Maybe Int)
square { col, row } board =
    let
        lowerCol =
            (col // 3) * 3

        lowerRow =
            (row // 3) * 3
    in
    List.range lowerCol (lowerCol + 2)
        |> List.concatMap
            (\col ->
                List.range lowerRow (lowerRow + 2)
                    |> List.map (\row -> { row = row, col = col })
                    |> List.map (flip get board)
            )


rows : Board -> List (List (Maybe Int))
rows board =
    List.range 0 8
        |> List.map (flip row board)


get : Position -> Board -> Maybe Int
get position (Board board) =
    Array.get (toOffset position) board
        |> join


set : Position -> Int -> Board -> Board
set position value (Board board) =
    Array.set (toOffset position) (Just value) board
        |> Board


stringifyRow : List (Maybe Int) -> String
stringifyRow =
    List.map (Maybe.map toString >> Maybe.withDefault " ")
        >> List.groupsOf 3
        >> List.map (String.join " ")
        >> String.join " | "


stringify : Board -> String
stringify =
    rows
        >> List.map stringifyRow
        >> List.groupsOf 3
        >> List.map (String.join "\n")
        >> List.intersperse "------+-------+------"
        >> String.join "\n"


join : Maybe (Maybe a) -> Maybe a
join maybe =
    case maybe of
        Just v ->
            v

        _ ->
            Nothing
