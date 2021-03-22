module Imports exposing (suite)

import Test exposing (..)

import Helpers exposing (..)

suite : Test
suite =
    describe "Normalize"
    [ test "shoud not normalize qualified imports" <|
            \_ ->
                givenElmCodeOf """import List

length : List.List -> Int                
length list = List.length list"""
                |> whenNormalize                
                |> thenContains """import List
IDENTIFIER_1 : List.List -> Int
IDENTIFIER_1 IDENTIFIER_2 =
List.length IDENTIFIER_2"""

    , test "shoud not normalize qualified aliased imports" <|
            \_ ->
                givenElmCodeOf """import List as L

length : L.List -> Int                
length list = L.length list"""
                |> whenNormalize                
                |> thenContains """import List as L
IDENTIFIER_1 : L.List -> Int
IDENTIFIER_1 IDENTIFIER_2 =
L.length IDENTIFIER_2"""        


    , test "shoud not normalize open imports" <|
            \_ ->
                givenElmCodeOf """import List exposing (List, length)

shadowLength : List -> Int                
shadowLength list = length list"""
                |> whenNormalize                
                |> thenContains """import List exposing (List, length)
IDENTIFIER_1 : List -> Int
IDENTIFIER_1 IDENTIFIER_2 =
length IDENTIFIER_2"""        
    ]
