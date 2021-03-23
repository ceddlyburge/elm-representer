module Normalization exposing (State, getIdentifierMapping, initialize, normalize)

import Dict as Dict exposing (Dict)


type State
    = State (Dict String String) Int


initialize : List String -> State
initialize customReservedWords =
    let
        reservedWords =
            [ ( "Int", "Int" )
            , ( "Float", "Float" )
            , ( "toFloat", "toFloat" )
            , ( "round", "round" )
            , ( "floor", "floor" )
            , ( "ceiling", "ceiling" )
            , ( "truncate", "truncate" )
            , ( "max", "max" )
            , ( "min", "min" )
            , ( "compare", "compare" )
            , ( "Order", "Order" )
            , ( "GT", "GT" )
            , ( "EQ", "EQ" )
            , ( "LT", "LT" )
            , ( "Bool", "Bool" )
            , ( "True", "True" )
            , ( "False", "False" )
            , ( "not", "not" )
            , ( "xor", "xor" )
            , ( "modBy", "modBy" )
            , ( "remainderBy", "remainderBy" )
            , ( "negate", "negate" )
            , ( "abs", "abs" )
            , ( "clamp", "clamp" )
            , ( "sqrt", "sqrt" )
            , ( "logBase", "logBase" )
            , ( "e", "e" )
            , ( "pi", "pi" )
            , ( "cos", "cos" )
            , ( "sin", "sin" )
            , ( "tan", "tan" )
            , ( "acos", "acos" )
            , ( "asin", "asin" )
            , ( "atan", "atan" )
            , ( "atan2", "atan2" )
            , ( "degrees", "degrees" )
            , ( "radians", "radians" )
            , ( "turns", "turns" )
            , ( "toPolar", "toPolar" )
            , ( "fromPolar", "fromPolar" )
            , ( "isNan", "isNan" )
            , ( "isInfinite", "isInfinite" )
            , ( "identity", "identity" )
            , ( "always", "always" )
            , ( "Never", "Never" )
            , ( "never", "never" )
            , ( "List", "List" )
            , ( "Maybe", "Maybe" )
            , ( "Just", "Just" )
            , ( "Result", "Result" )
            , ( "Ok", "Ok" )
            , ( "Err", "Err" )
            , ( "String", "String" )
            , ( "Char", "Char" )
            , ( "Tuple", "Tuple" )
            , ( "Debug", "Debug" )
            , ( "Program", "Program" )
            , ( "Cmd", "Cmd" )
            , ( "Sub", "Sub" )
            , ( "number", "number" )
            , ( "comparable", "comparable" )
            , ( "appendable", "appendable" )
            , ( "compappend", "compappend" )
            ]
                ++ List.map (\s -> ( s, s )) customReservedWords

        initialIdentifierMapping =
            Dict.fromList reservedWords
    in
    State initialIdentifierMapping 1


normalize : State -> String -> ( State, String )
normalize state original =
    let
        (State identifierMapping uniqueInt) =
            state

        existingMapping =
            Dict.get original identifierMapping
    in
    case existingMapping of
        Just normalizedIdentifier ->
            ( state, normalizedIdentifier )

        Nothing ->
            let
                newNormalizedIdentifier =
                    "IDENTIFIER_" ++ String.fromInt uniqueInt
            in
            ( State
                (Dict.insert original newNormalizedIdentifier identifierMapping)
                (uniqueInt + 1)
            , newNormalizedIdentifier
            )


getIdentifierMapping : State -> Dict String String
getIdentifierMapping (State identifierMapping _) =
    identifierMapping
