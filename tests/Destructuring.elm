module Destructuring exposing (suite)

import Test exposing (..)

import Helpers exposing (..)

suite : Test
suite =
    describe "Normalize"
    [ test "shoud normalize destructuring" <|
            \_ ->
                givenElmCodeOf "{name, age} = person"
                |> whenNormalize                
                |> thenContains "{IDENTIFIER_1, IDENTIFIER_2} =\nIDENTIFIER_3"

    ]
