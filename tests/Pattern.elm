module Pattern exposing (suite)

import Helpers exposing (..)
import Test exposing (..)


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
                givenElmCodeOf "shadowIdentity x :: xs =\n    x :: xs"
                    |> whenNormalize
                    |> thenContains "IDENTIFIER_1 IDENTIFIER_2 :: IDENTIFIER_3 =\nIDENTIFIER_2 :: IDENTIFIER_3"
        , test "shoud normalize list pattern" <|
            \_ ->
                givenElmCodeOf "add [x1, x2] =\n    x1 + x2"
                    |> whenNormalize
                    |> thenContains "IDENTIFIER_1 [IDENTIFIER_2, IDENTIFIER_3] =\nIDENTIFIER_2 + IDENTIFIER_3"
        , test "shoud normalize named pattern" <|
            \_ ->
                givenElmCodeOf "value (Node a) =\n    a"
                    |> whenNormalize
                    |> thenContains "IDENTIFIER_1 ( Node IDENTIFIER_2 ) =\nIDENTIFIER_2"
        , test "shoud normalize as pattern" <|
            \_ ->
                givenElmCodeOf "shadowIdentity ((_, _) as tuple) =\n    tuple"
                    |> whenNormalize
                    |> thenContains "IDENTIFIER_1 ( (_, _) as IDENTIFIER_2 ) =\nIDENTIFIER_2"
        ]
