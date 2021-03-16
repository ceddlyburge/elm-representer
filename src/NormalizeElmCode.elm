module NormalizeElmCode exposing (normalize)

import Elm.Parser
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.Expression exposing (Expression, Function)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Writer exposing (writeFile, write)
import Elm.Processing exposing (init, process)
import Elm.Writer exposing (writeFile)
import Normalization 
import List
import Dict as Dict exposing (Dict)
import Maybe as Maybe

-- todo
-- add pull request to elm-syntax repo about the process init thing which is hard to work out

-- check for pre existing items in the identifer mapping
-- add tests for type alias

-- convention variable names to 
--  'state'
--  'original'

-- do a round trip at the end of this to make sure that the normalization
-- code is working. This will catch the potential error when a returned
-- Normalization.State is ignored instead of being passed in to normalize
normalize : String -> (Dict String String, String)
normalize unNormalised =
    case Elm.Parser.parse unNormalised of
        Err error ->
            (Dict.empty, "Failed: " ++ Debug.toString error)
        Ok rawFile ->
            normalizeElmFile 
                Normalization.initialize
                (process init rawFile) 
            |> Tuple.mapFirst Normalization.getIdentifierMapping
            |> Tuple.mapSecond (writeFile >> write)


normalizeElmFile : Normalization.State -> File -> (Normalization.State, File)
normalizeElmFile state original = 
    let
        (state2, normalizedDeclarations) =
            normalizeNodes normalizeNodeDeclaration state original.declarations

        normalizedFile = 
            File
                original.moduleDefinition
                original.imports
                normalizedDeclarations
                []
                
    in
        ( state2
        , normalizedFile)

normalizeNodeDeclaration : Normalization.State -> Node Declaration -> (Normalization.State, Node Declaration)
normalizeNodeDeclaration state original =
    normalizeNode normalizeDeclaration state original

normalizeDeclaration : Normalization.State -> Declaration -> (Normalization.State, Declaration)
normalizeDeclaration state declaration =
    case declaration of
        Declaration.AliasDeclaration typeAlias ->
            let
                normalizedTypeAlias = normalizeTypeAlias state typeAlias
            in
                ( Tuple.first normalizedTypeAlias
                , Declaration.AliasDeclaration (Tuple.second normalizedTypeAlias)
                )

        _ ->
            (state, declaration)

-- normaliseDeclaration : IdentifierMapping ->  Declaration -> WithIdentifierMapping Declaration
-- normaliseDeclaration identifierMapping decl =
--         case decl of
--             Declaration.FunctionDeclaration function ->
--                 WithIdentifierMapping
--                     identifierMapping
--                     <| Declaration.FunctionDeclaration (normaliseFunctionDeclaration function)

--             Declaration.CustomTypeDeclaration typeDeclaration ->
--                 WithIdentifierMapping
--                     identifierMapping
--                     <| Declaration.CustomTypeDeclaration (normaliseTypeDeclaration typeDeclaration)

--             Declaration.PortDeclaration p ->
--                 WithIdentifierMapping
--                     identifierMapping
--                     <| Declaration.PortDeclaration p

--             Declaration.InfixDeclaration inf ->
--                 WithIdentifierMapping
--                     identifierMapping
--                     <| Declaration.InfixDeclaration inf
            
--             Declaration.Destructuring x y ->
--                 WithIdentifierMapping
--                     identifierMapping
--                     <| Declaration.Destructuring x y

normalizeTypeAlias : Normalization.State -> TypeAlias -> (Normalization.State, TypeAlias)
normalizeTypeAlias state original =
    let
        (state2, normalizedName) =
            normalizeNodeString 
                state 
                original.name
        
        (state3, normalizedGenerics) = 
            normalizeNodeStrings 
                state2
                original.generics
        
        (state4, normalizedTypeAnnotation) = 
            normalizeNodeTypeAnnotation 
                state3
                original.typeAnnotation
        
        typeAlias = 
            TypeAlias
                Maybe.Nothing
                normalizedName
                normalizedGenerics
                normalizedTypeAnnotation      
    in
        ( state4, typeAlias )


normalizeNodeTypeAnnotation : Normalization.State -> Node TypeAnnotation -> (Normalization.State, Node TypeAnnotation)
normalizeNodeTypeAnnotation state original =
    normalizeNode normalizeTypeAnnotation state original

normalizeNodeTypeAnnotations : Normalization.State -> List (Node TypeAnnotation) -> (Normalization.State, List (Node TypeAnnotation))
normalizeNodeTypeAnnotations state original =
    normalizeNodes normalizeNodeTypeAnnotation state original

normalizeTypeAnnotation : Normalization.State -> TypeAnnotation -> (Normalization.State, TypeAnnotation)
normalizeTypeAnnotation state typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.Record original ->
            let
                normalized = normalizeNodeRecordFields state original
            in
                ( Tuple.first normalized
                , TypeAnnotation.Record (Tuple.second normalized)
                )
        
        TypeAnnotation.Tupled original ->
            let
                normalized = normalizeNodeTypeAnnotations state original
            in
                ( Tuple.first normalized
                , TypeAnnotation.Tupled (Tuple.second normalized)
                )

        TypeAnnotation.Typed originalName originalTypes ->
            let
                (state2, normalizedName) = normalizeNodeTypeName state originalName
                (state3, normalizedTypes) = normalizeNodes normalizeNodeTypeAnnotation state2 originalTypes
            in
                ( state3
                , TypeAnnotation.Typed normalizedName normalizedTypes
                )
        
        TypeAnnotation.GenericRecord originalName originalRecordDefinition ->
            let
                (state2, normalizedName) = normalizeNodeString state originalName
                (state3, normalizedRecordDefinition) = normalizeNodeRecordDefinition state2 originalRecordDefinition
            in
                ( state3
                , TypeAnnotation.GenericRecord normalizedName normalizedRecordDefinition
                )

        _ ->
            (state, typeAnnotation)

normalizeNodeRecordDefinition: Normalization.State -> Node TypeAnnotation.RecordDefinition -> (Normalization.State, Node TypeAnnotation.RecordDefinition)
normalizeNodeRecordDefinition state original =
    normalizeNode normalizeRecordDefinition state original

--type alias RecordDefinition =
--    List (Node RecordField)
normalizeRecordDefinition : Normalization.State -> TypeAnnotation.RecordDefinition -> (Normalization.State, TypeAnnotation.RecordDefinition)
normalizeRecordDefinition state original =
    normalizeNodeRecordFields state original


normalizeNodeRecordField: Normalization.State -> Node TypeAnnotation.RecordField -> (Normalization.State, Node TypeAnnotation.RecordField)
normalizeNodeRecordField state original =
    normalizeNode normalizeRecordField state original

normalizeNodeRecordFields: Normalization.State -> List (Node TypeAnnotation.RecordField) -> (Normalization.State, List (Node TypeAnnotation.RecordField))
normalizeNodeRecordFields state original =
    normalizeNodes normalizeNodeRecordField state original

normalizeRecordField : Normalization.State -> TypeAnnotation.RecordField -> (Normalization.State, TypeAnnotation.RecordField)
normalizeRecordField state original =
    let
        (state2, normalizedName) =
            normalizeNodeString 
                state 
                (Tuple.first original)
        
        (state3, normalizedTypeAnnotation) = 
            normalizeNodeTypeAnnotation
                state2
                (Tuple.second original)

        recordField = (normalizedName, normalizedTypeAnnotation)        
    in
        ( state3, recordField )


normalizeNodeTypeName : Normalization.State -> Node ( ModuleName, String ) -> (Normalization.State, Node ( ModuleName, String ))
normalizeNodeTypeName state original =
    normalizeNode normalizeTypeName state original

normalizeTypeName : Normalization.State -> ( ModuleName, String ) -> (Normalization.State, ( ModuleName, String ))
normalizeTypeName state (originalModuleName, originalTypeName) =
    let
        (state2, normalizedTypeName) =
            normalizeString 
                state 
                originalTypeName

        typeName = (originalModuleName, normalizedTypeName)        
    in
        ( state2, typeName )


{-| Custom type for different type annotations. For example:
  - `Var`: `a`
  - `Type`: `Maybe (Int -> String)`
  - `Tuples`: `(a, b, c)` or Unit `()`
  - `Record`: `{ name : String }`
  - `ExtensionRecord`: `{ a | name : String }`
  - `FunctionTypeAnnotation`: `Int -> String`
-}
-- type TypeAnnotation
--     = Var String
--     | Type (Node ( ModuleName, String )) (List (Node TypeAnnotation))
--     | Tuple (List (Node TypeAnnotation))
--     | Record (List (Node RecordField))
--     | ExtensionRecord (Node String) (Node RecordField) (List (Node RecordField))
--     | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)



normalizeNodeString : Normalization.State -> Node String -> (Normalization.State, Node String)
normalizeNodeString state original =
    normalizeNode normalizeString state original

normalizeString : Normalization.State -> String -> (Normalization.State, String)
normalizeString =
    Normalization.normalize

normalizeNodeStrings : Normalization.State -> List (Node String) -> (Normalization.State, List (Node String))
normalizeNodeStrings state original =
    normalizeNodes normalizeNodeString state original

normalizeNode : 
    (Normalization.State -> a -> (Normalization.State, a)) 
    -> Normalization.State 
    -> Node a 
    -> (Normalization.State, Node a)
normalizeNode normalizer state original =
    let
        normalized =
            Node.map 
                (normalizer state) 
                original
    in
        ( Node.value normalized |> Tuple.first
        , Node.map Tuple.second normalized)

normalizeNodes : 
    (Normalization.State -> Node a -> (Normalization.State, Node a)) 
    -> Normalization.State 
    -> List (Node a) 
    -> (Normalization.State, List (Node a))
normalizeNodes normalizer state original =
    List.foldl
        (normalizeAccumulateNode normalizer) 
        (state, []) 
        original

normalizeAccumulateNode : 
    (Normalization.State -> Node a -> (Normalization.State, Node a)) 
    -> Node a 
    -> (Normalization.State, List (Node a)) 
    -> (Normalization.State, List (Node a))
normalizeAccumulateNode normalizer original (state, normalizedNodes) =
    let
        (nextState, normalized) = normalizer state original
    in
        ( nextState
        , normalizedNodes ++ [normalized]
        )
