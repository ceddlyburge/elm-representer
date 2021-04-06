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
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Writer exposing (write, writeFile)
import Normalization
import NormalizeElmCodeHelpers exposing (..)
import NormalizePattern exposing (..)
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


normalizeFunction : Normalization.State -> Function -> ( Normalization.State, Function )
normalizeFunction state original =
    let
        ( state2, normalizedSignature ) =
            normalizeMaybe
                normalizeNodeSignature
                state
                original.signature

        ( state3, normalizedFunctionImplementation ) =
            normalizeNodeFunctionImplementation
                state2
                original.declaration

        normalizedFunction =
            Function
                Maybe.Nothing
                normalizedSignature
                normalizedFunctionImplementation
    in
    ( state3, normalizedFunction )


normalizeNodeSignature : Normalization.State -> Node Signature -> ( Normalization.State, Node Signature )
normalizeNodeSignature state original =
    normalizeNode normalizeSignature state original


normalizeSignature : Normalization.State -> Signature -> ( Normalization.State, Signature )
normalizeSignature state original =
    let
        ( state2, normalizedName ) =
            normalizeNodeString
                state
                original.name

        ( state3, normalizedTypeAnnotation ) =
            normalizeNodeTypeAnnotation
                state2
                original.typeAnnotation

        normalizedSignature =
            Signature
                normalizedName
                normalizedTypeAnnotation
    in
    ( state3, normalizedSignature )


normalizeNodeFunctionImplementation : Normalization.State -> Node FunctionImplementation -> ( Normalization.State, Node FunctionImplementation )
normalizeNodeFunctionImplementation state original =
    normalizeNode normalizeFunctionImplementation state original


normalizeFunctionImplementation : Normalization.State -> FunctionImplementation -> ( Normalization.State, FunctionImplementation )
normalizeFunctionImplementation state original =
    let
        ( state2, normalizedName ) =
            normalizeNodeString
                state
                original.name

        ( state3, normalizedArguments ) =
            normalizeNodePatterns
                state2
                original.arguments

        ( state4, normalizedExpression ) =
            normalizeNodeExpression
                state3
                original.expression

        normalizedFunctionImplementation =
            FunctionImplementation
                normalizedName
                normalizedArguments
                normalizedExpression
    in
    ( state4, normalizedFunctionImplementation )


normalizeNodeExpression : Normalization.State -> Node Expression -> ( Normalization.State, Node Expression )
normalizeNodeExpression state original =
    normalizeNode normalizeExpression state original


normalizeNodeExpressions : Normalization.State -> List (Node Expression) -> ( Normalization.State, List (Node Expression) )
normalizeNodeExpressions state original =
    normalizeNodes normalizeNodeExpression state original


normalizeExpression : Normalization.State -> Expression -> ( Normalization.State, Expression )
normalizeExpression state originalExpression =
    case originalExpression of
        UnitExpr ->
            ( state, UnitExpr )

        Application original ->
            let
                ( state2, normalized ) =
                    normalizeNodeExpressions state original
            in
            ( state2, Application normalized )

        OperatorApplication op dir left right ->
            let
                ( state2, normalizedLeft ) =
                    normalizeNodeExpression state left

                ( state3, normalizedRight ) =
                    normalizeNodeExpression state2 right

                normalized =
                    OperatorApplication
                        op
                        dir
                        normalizedLeft
                        normalizedRight
            in
            ( state3, normalized )

        FunctionOrValue originalModuleName originalName ->
            let
                ( state2, normalized ) =
                    normalizeString state originalName
            in
            if List.isEmpty originalModuleName then
                ( state2, FunctionOrValue originalModuleName normalized )

            else
                ( state, FunctionOrValue originalModuleName originalName )

        IfBlock c t e ->
            let
                ( state2, normalizedCondition ) =
                    normalizeNodeExpression state c

                ( state3, normalizedThen ) =
                    normalizeNodeExpression state2 t

                ( state4, normalizedElse ) =
                    normalizeNodeExpression state3 e

                normalized =
                    IfBlock
                        normalizedCondition
                        normalizedThen
                        normalizedElse
            in
            ( state4, normalized )

        PrefixOperator original ->
            ( state, PrefixOperator original )

        Operator original ->
            ( state, Operator original )

        Hex original ->
            ( state, Hex original )

        Integer original ->
            ( state, Integer original )

        Floatable original ->
            ( state, Floatable original )

        Negation original ->
            ( state, Negation original )

        Literal original ->
            ( state, Literal original )

        CharLiteral original ->
            ( state, CharLiteral original )

        TupledExpression original ->
            let
                ( state2, normalized ) =
                    normalizeNodeExpressions state original
            in
            ( state2, TupledExpression normalized )

        ListExpr original ->
            let
                ( state2, normalized ) =
                    normalizeNodeExpressions state original
            in
            ( state2, ListExpr normalized )

        ParenthesizedExpression original ->
            let
                ( state2, normalized ) =
                    normalizeNodeExpression state original
            in
            ( state2, ParenthesizedExpression normalized )

        LetExpression original ->
            let
                ( state2, normalizedDeclarations ) =
                    normalizeNodeLetDeclarations state original.declarations

                ( state3, normalizedExpression ) =
                    normalizeNodeExpression state2 original.expression

                normalized =
                    LetExpression <|
                        LetBlock
                            normalizedDeclarations
                            normalizedExpression
            in
            ( state3, normalized )

        CaseExpression original ->
            let
                ( state2, normalized ) =
                    normalizeCaseBlock state original
            in
            ( state2, CaseExpression normalized )

        LambdaExpression original ->
            let
                ( state2, normalizedArguments ) =
                    normalizeNodePatterns state original.args

                ( state3, normalizedExpression ) =
                    normalizeNodeExpression state2 original.expression

                normalized =
                    LambdaExpression <|
                        Lambda
                            normalizedArguments
                            normalizedExpression
            in
            ( state3, normalized )

        RecordAccess exp name ->
            let
                ( state2, normalizedExpression ) =
                    normalizeNodeExpression state exp

                ( state3, normalizedName ) =
                    normalizeNodeString state2 name

                normalized =
                    RecordAccess
                        normalizedExpression
                        normalizedName
            in
            ( state3, normalized )

        RecordAccessFunction original ->
            let
                ( state2, normalized ) =
                    normalizeString state original
            in
            ( state2, RecordAccessFunction normalized )

        RecordExpr original ->
            let
                ( state2, normalized ) =
                    normalizeNodeRecordSetters state original
            in
            ( state2, RecordExpr normalized )

        RecordUpdateExpression name updates ->
            let
                ( state2, normalizedName ) =
                    normalizeNodeString state name

                ( state3, normalizedUpdates ) =
                    normalizeNodeRecordSetters state2 updates

                normalized =
                    RecordUpdateExpression
                        normalizedName
                        normalizedUpdates
            in
            ( state3, normalized )

        GLSLExpression original ->
            ( state, GLSLExpression original )


normalizeCaseBlock : Normalization.State -> CaseBlock -> ( Normalization.State, CaseBlock )
normalizeCaseBlock state original =
    let
        ( state2, normalizedExpression ) =
            normalizeNodeExpression state original.expression

        ( state3, normalizedCases ) =
            normalizeCases state2 original.cases

        normalized =
            CaseBlock normalizedExpression normalizedCases
    in
    ( state3, normalized )


normalizeNodeRecordSetter : Normalization.State -> Node RecordSetter -> ( Normalization.State, Node RecordSetter )
normalizeNodeRecordSetter state original =
    normalizeNode normalizeRecordSetter state original


normalizeNodeRecordSetters : Normalization.State -> List (Node RecordSetter) -> ( Normalization.State, List (Node RecordSetter) )
normalizeNodeRecordSetters state original =
    normalizeNodes normalizeNodeRecordSetter state original


normalizeRecordSetter : Normalization.State -> RecordSetter -> ( Normalization.State, RecordSetter )
normalizeRecordSetter state ( originalName, originalExpression ) =
    let
        ( state2, normalizedName ) =
            normalizeNodeString state originalName

        ( state3, normalizedExpression ) =
            normalizeNodeExpression state2 originalExpression

        normalized =
            ( normalizedName, normalizedExpression )
    in
    ( state3, normalized )


normalizeCases : Normalization.State -> List Case -> ( Normalization.State, List Case )
normalizeCases state original =
    normalizeList normalizeCase state original


normalizeCase : Normalization.State -> Case -> ( Normalization.State, Case )
normalizeCase state ( originalPattern, originalExpression ) =
    let
        ( state2, normalizedPattern ) =
            normalizeNodePattern state originalPattern

        ( state3, normalizedExpression ) =
            normalizeNodeExpression state2 originalExpression

        normalized =
            ( normalizedPattern, normalizedExpression )
    in
    ( state3, normalized )


normalizeNodeLetDeclaration : Normalization.State -> Node LetDeclaration -> ( Normalization.State, Node LetDeclaration )
normalizeNodeLetDeclaration state original =
    normalizeNode normalizeLetDeclaration state original


normalizeNodeLetDeclarations : Normalization.State -> List (Node LetDeclaration) -> ( Normalization.State, List (Node LetDeclaration) )
normalizeNodeLetDeclarations state original =
    normalizeNodes normalizeNodeLetDeclaration state original


normalizeLetDeclaration : Normalization.State -> LetDeclaration -> ( Normalization.State, LetDeclaration )
normalizeLetDeclaration state originalLetDeclaration =
    case originalLetDeclaration of
        LetFunction original ->
            let
                ( state2, normalized ) =
                    normalizeFunction state original
            in
            ( state2, LetFunction normalized )

        LetDestructuring originalPattern originalExpression ->
            let
                ( state2, normalizedPattern ) =
                    normalizeNodePattern state originalPattern

                ( state3, normalizedExpression ) =
                    normalizeNodeExpression state2 originalExpression
            in
            ( state3, LetDestructuring normalizedPattern normalizedExpression )


normalizeType : Normalization.State -> Type -> ( Normalization.State, Type )
normalizeType state original =
    let
        ( state2, normalizedName ) =
            normalizeNodeString
                state
                original.name

        ( state3, normalizedGenerics ) =
            normalizeNodeStrings
                state2
                original.generics

        ( state4, normalizedValueConstructors ) =
            normalizeNodeValueConstructors
                state3
                original.constructors

        normalizedType =
            Type
                Maybe.Nothing
                normalizedName
                normalizedGenerics
                normalizedValueConstructors
    in
    ( state4, normalizedType )


normalizeNodeValueConstructor : Normalization.State -> Node ValueConstructor -> ( Normalization.State, Node ValueConstructor )
normalizeNodeValueConstructor state original =
    normalizeNode normalizeValueConstructor state original


normalizeNodeValueConstructors : Normalization.State -> List (Node ValueConstructor) -> ( Normalization.State, List (Node ValueConstructor) )
normalizeNodeValueConstructors state original =
    normalizeNodes normalizeNodeValueConstructor state original


normalizeValueConstructor : Normalization.State -> ValueConstructor -> ( Normalization.State, ValueConstructor )
normalizeValueConstructor state original =
    let
        ( state2, normalizedName ) =
            normalizeNodeString state original.name

        ( state3, normalizedArguments ) =
            normalizeNodeTypeAnnotations state2 original.arguments

        normalizedValueConstructor =
            ValueConstructor
                normalizedName
                normalizedArguments
    in
    ( state3, normalizedValueConstructor )
