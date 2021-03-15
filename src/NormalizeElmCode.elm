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
import Elm.Writer exposing (writeFile, write)
import Elm.Processing exposing (init, process)
import Elm.Writer exposing (writeFile)
import Normalization 
import List
import Dict as Dict exposing (Dict)
import Maybe as Maybe exposing (Maybe)

-- todo

-- convention variable names to 
--  'state'
--  'original'

-- add tests for type alias

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

normalizeTypeAnnotation : Normalization.State -> TypeAnnotation -> (Normalization.State, TypeAnnotation)
normalizeTypeAnnotation state typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.Record original ->
            let
                normalized = normalizeNodes normalizeNodeRecordField state original
            in
                ( Tuple.first normalized
                , TypeAnnotation.Record (Tuple.second normalized)
                )

        _ ->
            (state, typeAnnotation)

normalizeNodeRecordField : Normalization.State -> Node TypeAnnotation.RecordField -> (Normalization.State, Node TypeAnnotation.RecordField)
normalizeNodeRecordField state original =
    normalizeNode normalizeRecordField state original

normalizeRecordField : Normalization.State -> TypeAnnotation.RecordField -> (Normalization.State, TypeAnnotation.RecordField)
normalizeRecordField state original =
    (state, original)

{-| Custom type for different type annotations. For example:
  - `Var`: `a`
  - `Type`: `Maybe (Int -> String)`
  - `Tuples`: `(a, b, c)` or Unit `()`
  - `Record`: `{ name : String }`
  - `ExtensionRecord`: `{ a | name : String }`
  - `GenericRecord`: `{ a | name : String}`
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
    normalizeNode  Normalization.normalize state original

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
        , normalized :: normalizedNodes
        )
