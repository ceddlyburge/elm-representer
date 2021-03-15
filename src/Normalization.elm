module Normalization exposing (State, initialize, normalize, getIdentifierMapping)

import Dict as Dict exposing (Dict)

type State =
    State (Dict String String) Int

initialize : State
initialize =
    State Dict.empty 1

normalize : State -> String -> (State, String)
normalize (State identifierMapping uniqueInt) original =
    let
        normalized = "IDENTIFIER_" ++ String.fromInt uniqueInt
        nextState = 
           State
                (Dict.insert original normalized identifierMapping)
                (uniqueInt + 1)
    in
        (nextState, normalized)

getIdentifierMapping : State -> Dict String String
getIdentifierMapping (State identifierMapping _) = 
    identifierMapping