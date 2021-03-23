module Imports exposing (suite)

import Helpers exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Normalize"
        [ test "shoud not normalize qualified imports" <|
            \_ ->
                givenElmCodeOf """import Dict

length : Dict.Dict -> Int                
length list = Dict.length list"""
                    |> whenNormalize
                    |> thenContains """import Dict
IDENTIFIER_1 : Dict.Dict -> Int
IDENTIFIER_1 IDENTIFIER_2 =
Dict.length IDENTIFIER_2"""
        , test "shoud not normalize qualified aliased imports" <|
            \_ ->
                givenElmCodeOf """import Dict as D

length : D.Dict -> Int                
length list = D.length list"""
                    |> whenNormalize
                    |> thenContains """import Dict as D
IDENTIFIER_1 : D.Dict -> Int
IDENTIFIER_1 IDENTIFIER_2 =
D.length IDENTIFIER_2"""
        , test "shoud not normalize open imports" <|
            \_ ->
                givenElmCodeOf """import Dict exposing (Dict, length)

shadowLength : Dict -> Int                
shadowLength list = length list"""
                    |> whenNormalize
                    |> thenContains """import Dict exposing (Dict, length)
IDENTIFIER_1 : Dict -> Int
IDENTIFIER_1 IDENTIFIER_2 =
length IDENTIFIER_2"""
        ]
