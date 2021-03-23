module Function exposing (suite)

import Helpers exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Normalize"
        [ test "shoud normalize Name and parameters of functions" <|
            \_ ->
                givenElmCodeOf "identityFunction x =\n    x"
                    |> whenNormalize
                    |> thenContains "IDENTIFIER_1 IDENTIFIER_2 =\nIDENTIFIER_2"
        , test "shoud normalize function signature" <|
            \_ ->
                givenElmCodeOf """identityFunction : a -> a
identityFunction x =
    x"""
                    |> whenNormalize
                    |> thenContains """IDENTIFIER_1 : IDENTIFIER_2 -> IDENTIFIER_2
IDENTIFIER_1 IDENTIFIER_3 =
IDENTIFIER_3"""
        ]
