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

-- The output from Elm-Syntax isn't the same as Elm-Format, and also isn't always
-- consistent, so this function tries to make it as consistent as possile, although
-- it does turn the code in to illegal Elm by removing required whitespace.
-- Hopefully the tradeoff is worth it
thenContains : String -> (Dict String String, String) -> Expectation
thenContains expected normalizationResult =
    let
        normalized = 
            Tuple.second normalizationResult
            -- remove added boilerplate
            |> String.replace boilerplate ""
            -- keep line breaks, but compress all other multiple whitespace to a single whitespace
            |> String.lines
            |> List.map String.Extra.clean
            |> List.filter (String.isEmpty >> not) -- remove blank lines
            |> String.join "\n"
            |> String.trim
    in 
        Expect.Extra.match (Expect.Extra.stringPattern expected) normalized

boilerplate : String
boilerplate = 
    """module Boilerplate exposing (..)

"""
