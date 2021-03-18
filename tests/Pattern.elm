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
                givenElmCodeOf "identity x :: xs =\n    x :: xs"
                |> whenNormalize                
                |> thenContains "IDENTIFIER_1 IDENTIFIER_2 :: IDENTIFIER_3 =\nIDENTIFIER_2 :: IDENTIFIER_3"

    , test "shoud normalize list pattern" <|
            \_ ->
                givenElmCodeOf "add [x1, x2] =\n    x1 + x2"
                |> whenNormalize                
                |> thenContains "IDENTIFIER_1 [IDENTIFIER_2, IDENTIFIER_3] =\nIDENTIFIER_2 + IDENTIFIER_3"



    , test "shoud normalize names pattern" <|
            \_ ->
                givenElmCodeOf "value (Node a) =\n    a"
                |> whenNormalize                
                |> thenContains "IDENTIFIER_1 ( Node IDENTIFIER_2 ) =\nIDENTIFIER_2"

    -- use for case expression when done
    -- , test "shoud normalize named pattern" <|
    --         \_ ->
    --             givenElmCodeOf """default1 maybeX =
    -- case maybeX of
    --     Just x ->
    --         x
        
    --     Nothing ->
    --         1"""
    --             |> whenNormalize                
    --             |> thenContains """IDENTIFIER_1 IDENTIFIER_2 =
    -- case IDENTIFIER_2
    --     Just IDENTIFIER_3 ->
    --         IDENTIFIER_3
        
    --     Nothing ->
    --         1"""
                
    ]
