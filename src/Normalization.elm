module Normalization exposing (State, initialize, normalize, getIdentifierMapping)

import Dict as Dict exposing (Dict)

type State =
    State (Dict String String) Int

initialize : State
initialize =
    let
        reservedWords = 
            Dict.fromList 
            [ ("Bool","Bool")
            , ("String","String")
            , ("Char","Char")
            , ("Float","Float")
            , ("Int","Int")
            , ("number","number")
            , ("appendable","appendable")
            , ("comparable","comparable")
            , ("compappend","compappend")
            , ("True","True")
            , ("False","False")
            ]
    in
        State reservedWords 1

normalize : State -> String -> (State, String)
normalize state original =
    let
        (State identifierMapping uniqueInt) = state
        existingMapping = Dict.get original identifierMapping
    in
        case existingMapping of
            Just normalizedIdentifier ->
                (state, normalizedIdentifier)
            
            Nothing ->
                let
                    newNormalizedIdentifier = "IDENTIFIER_" ++ String.fromInt uniqueInt
                in
                    ( State
                        (Dict.insert original newNormalizedIdentifier identifierMapping)
                        (uniqueInt + 1)
                    , newNormalizedIdentifier)

getIdentifierMapping : State -> Dict String String
getIdentifierMapping (State identifierMapping _) = 
    identifierMapping