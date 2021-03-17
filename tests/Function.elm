module Function exposing (suite)

import Test exposing (..)

import Helpers exposing (..)

suite : Test
suite =
    describe "Normalize"
    [ test "shoud normalize Name and parameters of functions" <|
            \_ ->
                givenElmCodeOf "identityFunction identity =\n    identity"
                |> whenNormalize                
                |> thenContains "IDENTIFIER_1 IDENTIFIER_2 =\nIDENTIFIER_2"
        
    -- , test "shoud normalize generic type parameters of Custom Types" <|
    --         \_ ->
    --             givenElmCodeOf "type InputType a = InputType a String"
    --             |> whenNormalize                
    --             |> thenContains "type IDENTIFIER_1 IDENTIFIER_2\n=IDENTIFIER_1 IDENTIFIER_2 String"

    ]
