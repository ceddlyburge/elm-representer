module TypeAlias exposing (suite)

import Expect exposing (Expectation)
import Expect.Extra
--import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict exposing (Dict)

import NormalizeElmCode

-- extensible record
-- type alias Selectable a =
--     { a
--         | isSelected : Bool
--     }
-- and function type alias

suite : Test
suite =
    describe "Normalize"
    [
        test "shoud normalize Name of primitive Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias Name = String"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1  =\n    String"
        
        , test "shoud normalize Name and Type of Custom Type - Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias FirstName = Name"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1  =\n    IDENTIFIER_2"
        
        , skip <| test "shoud normalize Name and generic type parameters of Custom Type - Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias InputType a b = Result a b"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1 IDENTIFIER_2 =\n    Result IDENTIFIER_1 IDENTIFIER_2"
        
        , test "shoud normalize name of Tuple Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias InputType = (String, Int)"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1  =\n    (String, Int)"
        
        , test "shoud normalize parameter types of Tuple Type Alias'" <|
            \_ ->
                givenElmCodeOf "type alias InputType = (Name, Name)"
                |> whenNormalize                
                |> thenContains "type alias IDENTIFIER_1  =\n    (IDENTIFIER_2, IDENTIFIER_2)"

        , test "shoud normalize Name and Parameters of Record Types" <|
            \_ ->
                givenElmCodeOf """
type alias Person = 
    { name : Name
    , age : Int
    }
"""
                |> whenNormalize                
                |> thenContains """type alias IDENTIFIER_1  =
    {IDENTIFIER_2 : IDENTIFIER_3, IDENTIFIER_4 : Int}"""

        , test "shoud normalize Name and Parameters of Extensible Record Types" <|
            \_ ->
                givenElmCodeOf """
type alias Person a = 
    { a |
    name : String
    }
"""
                |> whenNormalize                
                |> thenContains """type alias IDENTIFIER_1 IDENTIFIER_2 =
    { IDENTIFIER_2 | IDENTIFIER_3 : String }"""

    ]


givenElmCodeOf: String -> String
givenElmCodeOf elmCode = 
    boilerplate ++ elmCode

whenNormalize : String -> (Dict String String, String)
whenNormalize =
    NormalizeElmCode.normalize

thenContains : String -> (Dict String String, String) -> Expectation
thenContains expected normalizationResult =
    let
        normalizedCode = Tuple.second normalizationResult
        normalizedCodeWithoutBoilerplate = String.replace boilerplate "" normalizedCode
    in 
        Expect.Extra.match (Expect.Extra.stringPattern expected) normalizedCodeWithoutBoilerplate
        --String.contains expected normalizedCode
        --|> Expect.true 
        --("Elm Code should contain \n" ++ expected ++ "\nbut is \n" ++ normalizedCodeWithoutBoilerplate ++ "\n")

boilerplate : String
boilerplate = 
    """module Boilerplate exposing (..)

"""
