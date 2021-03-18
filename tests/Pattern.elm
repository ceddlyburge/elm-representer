module Pattern exposing (suite)

import Test exposing (..)

import Helpers exposing (..)

suite : Test
suite =
    describe "Normalize"
    [ test "shoud normalize tuple pattern" <|
            \_ ->
                givenElmCodeOf "add (x, y) =\n    x + y"
                |> whenNormalize                
                |> thenContains "IDENTIFIER_1 (IDENTIFIER_2, IDENTIFIER_3) =\nIDENTIFIER_2 + IDENTIFIER_3"

    , test "shoud normalize record pattern" <|
            \_ ->
                givenElmCodeOf "add {x, y} =\n    x + y"
                |> whenNormalize                
                |> thenContains "IDENTIFIER_1 {IDENTIFIER_2, IDENTIFIER_3} =\nIDENTIFIER_2 + IDENTIFIER_3"
                
    , test "shoud normalize uncons pattern" <|
            \_ ->
                givenElmCodeOf "add x :: xs =\n    x :: xs"
                |> whenNormalize                
                |> thenContains "IDENTIFIER_1 IDENTIFIER_2 :: IDENTIFIER_3 =\nIDENTIFIER_2 :: IDENTIFIER_3"
    ]
