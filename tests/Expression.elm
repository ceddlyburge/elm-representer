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
        
    -- , test "shoud normalize generic type parameters of Custom Types" <|
    --         \_ ->
    --             givenElmCodeOf "type InputType a = InputType a String"
    --             |> whenNormalize                
    --             |> thenContains "type IDENTIFIER_1 IDENTIFIER_2\n=IDENTIFIER_1 IDENTIFIER_2 String"

    ]
