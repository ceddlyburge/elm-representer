module Destructuring exposing (suite)

import Helpers exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Normalize"
        [ test "shoud normalize destructuring" <|
            \_ ->
                givenElmCodeOf "{name, age} = person"
                    |> whenNormalize
                    |> thenContains "{IDENTIFIER_1, IDENTIFIER_2} =\nIDENTIFIER_3"
        ]
