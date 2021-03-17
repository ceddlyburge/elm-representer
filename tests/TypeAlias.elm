module TypeAlias exposing (suite)

import Test exposing (..)

import Helpers exposing (..)

suite : Test
suite =
    describe "Normalize"
    [
        test "shoud normalize Name of primitive Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias Name = String"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 =\nString"
        
        , test "shoud normalize Name and Type of Custom Type - Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias FirstName = Name"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 =\nIDENTIFIER_2"
        
        , test "shoud normalize Name and generic type parameters of Custom Type - Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias InputType a = Result a"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 IDENTIFIER_2 =\nIDENTIFIER_3 IDENTIFIER_2"
        
        , test "shoud normalize name of Tuple Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias InputType = (String, Int)"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 =\n(String, Int)"
        
        , test "shoud normalize parameter types of Tuple Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias InputType = (Name, Name)"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 =\n(IDENTIFIER_2, IDENTIFIER_2)"

        , test "shoud normalize types of Function Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias InputType = Name -> Name"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 =\nIDENTIFIER_2 -> IDENTIFIER_2"

        , test "shoud normalize Name and Parameters of Record Types" <|
            \_ ->
                givenElmCodeOf """
type alias Person = 
    { name : Name
    , age : Int
    }
"""
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 =\n{IDENTIFIER_2 : IDENTIFIER_3, IDENTIFIER_4 : Int}"

        , test "shoud normalize Name and Parameters of Extensible Record Types" <|
            \_ ->
                givenElmCodeOf """
type alias Person a = 
    { a |
    name : String
    }
"""
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 IDENTIFIER_2 =\n{ IDENTIFIER_2 | IDENTIFIER_3 : String }"

    ]
