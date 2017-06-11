module Example exposing (..)

import Array.Hamt as Array
import Expect
import Sudoku.Board as Board exposing (Board(..), Position)
import Test exposing (..)


columns : Test
columns =
    describe "columns"
        [ describe "Valid" (List.range 0 8 |> List.map (isValidColumnTest True validBoard))
        , describe "Invalid" (List.range 0 8 |> List.map (isValidColumnTest False invalidBoard1))
        , describe "More invalid" (List.range 0 8 |> List.map (isValidColumnTest False invalidBoard2))
        ]


rows : Test
rows =
    describe "rows"
        [ describe "Valid" (List.range 0 8 |> List.map (isValidRowTest True validBoard))
        , describe "Invalid" (List.range 0 8 |> List.map (isValidRowTest False invalidBoard1))
        , describe "More invalid" (List.range 0 8 |> List.map (isValidRowTest False invalidBoard2))
        ]


squares : Test
squares =
    describe "squares"
        [ describe "valid" (squareTest True validBoard)
        , describe "invalid" (squareTest False invalidBoard1)
        , describe "More invalid" (squareTest False invalidBoard2)
        ]


squareTest : Bool -> Board -> List Test
squareTest valid board =
    List.range 0 8
        |> List.concatMap
            (\row ->
                List.range 0 8
                    |> List.map
                        (\col ->
                            isValidSquareTest valid board row col
                        )
            )


isValidColumnTest : Bool -> Board -> Int -> Test
isValidColumnTest valid board col =
    test (toString col ++ " is valid") <|
        \_ ->
            Board.isValidColumn col board
                |> Expect.equal valid


isValidRowTest : Bool -> Board -> Int -> Test
isValidRowTest valid board row =
    test (toString row ++ " is valid") <|
        \_ ->
            Board.isValidRow row board
                |> Expect.equal valid


isValidSquareTest : Bool -> Board -> Int -> Int -> Test
isValidSquareTest valid board x y =
    test ("Square at " ++ toString x ++ "/" ++ toString y) <|
        \_ ->
            Board.isValidSquare (Position x y) board
                |> Expect.equal valid


invalidBoard1 : Board
invalidBoard1 =
    List.repeat 81 (Just 1)
        |> Array.fromList
        |> Board


invalidBoard2 : Board
invalidBoard2 =
    List.repeat 81 Nothing
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
                List.range x (x + 8)
                    |> List.map (\i -> ((x + i) % 9) + 1)
            )
        |> List.map Just
        |> Array.fromList
        |> Board
