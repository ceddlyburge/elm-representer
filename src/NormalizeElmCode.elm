module NormalizeElmCode exposing (normalize)

import Dict as Dict exposing (Dict)
import Elm.Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Writer exposing (write, writeFile)
import Normalization
import NormalizeDeclaration exposing (..)
import NormalizeElmCodeHelpers exposing (..)
import NormalizeExpression exposing (..)
import NormalizePattern exposing (..)
import NormalizeType exposing (..)
import NormalizeTypeAlias exposing (..)
import NormalizeTypeAnnotation exposing (..)
import Parser



-- todo
-- Run the example files as tests, just to ensure that they succeed (don't worry about the created text). This will ensure that the created code is valid and can be re parsed, and if we ever find problems we can add them to the examples.
-- Later: split up this file in to smaller files
-- Later: dockerise
--  run elm-format afterwards. the code created by elm-syntax can be a bit weird, and is more likely to change that the format that elm-format insists on. Although apparently elm-syntax-dsl might do a similar thing and be easier to integrate, so investigate that
-- Later: Think about using elm-syntax-dsl instead of elm-syntax. It uses the same types, but without the `Node a` type stuff, which should make things simpler and result in less code. I have looked at the code though, and in now I'm not sure it will help, as it just re exposes the elm-syntax types, instead of redefining similar types but wihtout the Node's
-- Later: create pull request with exercism elm-representer
-- Later: add code coverage
--  This isn't working at the moment, I think elm coverrage doesn't work with latest version of elm-test
-- Later: add pull request / issue to elm-syntax repo about the `process init` thing which is hard to work out
-- Later: add pull request / issue to elm-syntax about typeclasses being classified as generic types in the syntax
-- Later, probably never: normalize within scope (so 'a' can normalize to different values if it is defined in different scopes). This would improve the normalization, but the mapping format defined by exercism doesn't support it, so there probably isn't much point, and it would be harder to do


normalize : String -> Result String ( Dict String String, String )
normalize original =
    let
        ( firstState, firstNormalization ) =
            normalizeWithoutCheck original

        ( _, secondNormalization ) =
            normalizeWithoutCheck firstNormalization
    in
    if firstNormalization == secondNormalization then
        Ok ( firstState, firstNormalization )

    else
        Err <|
            "Inconsistency detected, which means there is a bug, please create an Issue with this output"
                ++ "\n\nFirst normalisation\n"
                ++ firstNormalization
                ++ "\n\nSecond normalisation\n"
                ++ secondNormalization


normalizeWithoutCheck : String -> ( Dict String String, String )
normalizeWithoutCheck original =
    case Elm.Parser.parse original of
        Err error ->
            ( Dict.empty, "Failed: " ++ Parser.deadEndsToString error )

        Ok rawFile ->
            process init rawFile
                |> normalizeElmFile
                |> Tuple.mapFirst Normalization.getIdentifierMapping
                |> Tuple.mapSecond (writeFile >> write)


normalizeElmFile : File -> ( Normalization.State, File )
normalizeElmFile original =
    let
        exportedNames =
            Module.exposingList (Node.value original.moduleDefinition)
                |> exposingNames

        importedNames =
            original.imports
                |> List.map Node.value
                |> List.concatMap importNames

        state =
            Normalization.initialize (exportedNames ++ importedNames)

        ( state2, normalizedDeclarations ) =
            normalizeNodes normalizeNodeDeclaration state original.declarations

        normalized =
            File
                original.moduleDefinition
                original.imports
                normalizedDeclarations
                []
    in
    ( state2, normalized )


importNames : Import -> List String
importNames theImport =
    let
        explicits =
            Maybe.map Node.value theImport.exposingList
                |> Maybe.map exposingNames
                |> Maybe.withDefault []

        aliasName =
            Maybe.map Node.value theImport.moduleAlias
                |> Maybe.map (String.join ".")
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    explicits ++ aliasName


exposingNames : Exposing -> List String
exposingNames theExposing =
    case theExposing of
        All _ ->
            []

        Explicit nodeExposings ->
            List.map Node.value nodeExposings
                |> List.map topLevelExposeName


topLevelExposeName : TopLevelExpose -> String
topLevelExposeName topLevelExpose =
    case topLevelExpose of
        InfixExpose name ->
            name

        FunctionExpose name ->
            name

        TypeOrAliasExpose name ->
            name

        TypeExpose { name } ->
            name


normalizeNodeDeclaration : Normalization.State -> Node Declaration -> ( Normalization.State, Node Declaration )
normalizeNodeDeclaration state original =
    normalizeNode normalizeDeclaration state original


normalizeDeclaration : Normalization.State -> Declaration -> ( Normalization.State, Declaration )
normalizeDeclaration state declaration =
    case declaration of
        Declaration.AliasDeclaration original ->
            let
                ( state2, normalized ) =
                    normalizeTypeAlias state original
            in
            ( state2, Declaration.AliasDeclaration normalized )

        Declaration.CustomTypeDeclaration original ->
            let
                ( state2, normalized ) =
                    normalizeType state original
            in
            ( state2, Declaration.CustomTypeDeclaration normalized )

        Declaration.FunctionDeclaration original ->
            let
                ( state2, normalized ) =
                    normalizeFunction state original
            in
            ( state2, Declaration.FunctionDeclaration normalized )

        Declaration.Destructuring originalPattern originalExpression ->
            let
                ( state2, normalizedPattern ) =
                    normalizeNodePattern state originalPattern

                ( state3, normalizedExpression ) =
                    normalizeNodeExpression state2 originalExpression

                normalized =
                    Declaration.Destructuring
                        normalizedPattern
                        normalizedExpression
            in
            ( state3, normalized )

        -- I don't think we need to worry about port declarations and infixes (which are for core packages only)
        -- but we can revisit later if needs be
        _ ->
            ( state, declaration )
