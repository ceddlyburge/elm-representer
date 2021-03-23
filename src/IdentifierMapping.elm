module IdentifierMapping exposing (WithIdentifierMapping, initialize, map, normalise, normaliseString, value)

import Dict



-- would probably want to to a "parse dont validate" style thing here and hide the uniqueInteger,
-- / make it an implementation detail of a module.
-- rename to normalise


type IdentifierMapping
    = IdentifierMapping (Dict.Dict String String) Int



-- rename to noraliser


type WithIdentifierMapping a
    = WithIdentifierMapping IdentifierMapping a



-- normaliseNodeString: WithIdentifierMapping (Node String) -> WithIdentifierMapping (Node String)
-- normaliseNodeString (WithIdentifierMapping (IdentifierMapping mapping uniqueInt) unNormalisedNodeString) =
--     let
--         unnormalisedString = Node.value unNormalisedNodeString
--         range = Node.range unNormalisedNodeString
--         normalisedString = "IDENTIFIER_" ++ String.fromInt uniqueInt
--         nextIdentifierMapping = Dict.insert unnormalisedString normalisedString mapping
--         normalisedNodeString = (Node range) unNormalisedNodeString
--     in
--         WithIdentifierMapping
--             ( IdentifierMapping
--                 nextIdentifierMapping
--                 (uniqueInt + 1)
--             )
--             normalisedNodeString


normalise : (a -> String) -> (String -> a) -> WithIdentifierMapping a -> WithIdentifierMapping a
normalise toString fromString (WithIdentifierMapping (IdentifierMapping mapping uniqueInt) unNormalised) =
    let
        normalised =
            "IDENTIFIER_" ++ String.fromInt uniqueInt

        nextIdentifierMapping =
            Dict.insert (toString unNormalised) normalised mapping
    in
    WithIdentifierMapping
        (IdentifierMapping
            nextIdentifierMapping
            (uniqueInt + 1)
        )
        (fromString normalised)


normaliseString : WithIdentifierMapping String -> WithIdentifierMapping String
normaliseString (WithIdentifierMapping (IdentifierMapping mapping uniqueInt) unNormalised) =
    let
        normalised =
            "IDENTIFIER_" ++ String.fromInt uniqueInt

        nextIdentifierMapping =
            Dict.insert unNormalised normalised mapping
    in
    WithIdentifierMapping
        (IdentifierMapping
            nextIdentifierMapping
            (uniqueInt + 1)
        )
        normalised


initialize : a -> WithIdentifierMapping a
initialize v =
    WithIdentifierMapping
        (IdentifierMapping Dict.empty 1)
        v


map : (a -> b) -> WithIdentifierMapping a -> WithIdentifierMapping b
map mapper (WithIdentifierMapping identifierMapping v) =
    WithIdentifierMapping
        identifierMapping
        (mapper v)


value : WithIdentifierMapping a -> a
value (WithIdentifierMapping _ v) =
    v
