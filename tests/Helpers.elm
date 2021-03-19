module Helpers exposing (givenElmCodeOf, whenNormalize, thenContains)

import Expect exposing (Expectation)
import Expect.Extra
import Test exposing (..)
import Dict exposing (Dict)
import String.Extra
import NormalizeElmCode

givenElmCodeOf: String -> String
givenElmCodeOf elmCode = 
    boilerplate ++ elmCode

whenNormalize : String -> (Dict String String, String)
whenNormalize =
    NormalizeElmCode.normalize

thenContains : String -> (Dict String String, String) -> Expectation
thenContains expected normalizationResult =
    let
        normalized = 
            Tuple.second normalizationResult
            -- removes added boilerplate
            |> String.replace boilerplate ""
            -- keep line breaks, but compresses all other multiple whitespace to a single whitespace
            |> String.lines
            |> List.map String.Extra.clean
            |> List.filter (String.isEmpty >> not)
            |> String.join "\n"
            |> String.trim
    in 
        Expect.Extra.match (Expect.Extra.stringPattern expected) normalized

boilerplate : String
boilerplate = 
    """module Boilerplate exposing (..)

"""
