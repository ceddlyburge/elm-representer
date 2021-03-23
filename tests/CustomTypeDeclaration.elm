module CustomTypeDeclaration exposing (suite)

import Helpers exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Normalize"
        [ test "shoud normalize Name and ValueConstructors of Custom Types" <|
            \_ ->
                givenElmCodeOf "type FirstName = FirstName String"
                    |> whenNormalize
                    |> thenContains "type IDENTIFIER_1\n=IDENTIFIER_1 String"
        , test "shoud normalize generic type parameters of Custom Types" <|
            \_ ->
                givenElmCodeOf "type InputType a = InputType a String"
                    |> whenNormalize
                    |> thenContains "type IDENTIFIER_1 IDENTIFIER_2\n=IDENTIFIER_1 IDENTIFIER_2 String"
        ]
