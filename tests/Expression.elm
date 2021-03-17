module Expression exposing (suite)

import Test exposing (..)

import Helpers exposing (..)

suite : Test
suite =
    describe "Normalize"
    [ test "shoud normalize operator application expressions" <|
            \_ ->
                givenElmCodeOf "add x y =\n    x + y"
                |> whenNormalize                
                |> thenContains "IDENTIFIER_1 IDENTIFIER_2 IDENTIFIER_3 =\nIDENTIFIER_2 + IDENTIFIER_3"

    -- test functionorvalue
        
    , test "shoud normalize if expressions " <|
            \_ ->
                givenElmCodeOf "max x y =\n    if x > y then x else y"
                |> whenNormalize                
                |> thenContains """IDENTIFIER_1 IDENTIFIER_2 IDENTIFIER_3 =
if IDENTIFIER_2 > IDENTIFIER_3 then
IDENTIFIER_2
else
IDENTIFIER_3"""

    ]
