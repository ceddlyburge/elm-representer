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
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.Expression exposing (Expression, Function)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Writer exposing (writeFile, write)
import Elm.Processing exposing (init, process)
import List
import Dict as Dict exposing (Dict)
import Elm.Writer exposing (writeFile)
import Normalization 

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
normalizeElmFile normalizer original = 
    let
        normalizedDeclarations = 
            List.foldl 
                normalizeDeclarations
                (normalizer, [])
                original.declarations
        normalizedFile = 
            File
                original.moduleDefinition
                original.imports
                (Tuple.second normalizedDeclarations)
                []
                
    in
        ( Tuple.first normalizedDeclarations
        , normalizedFile)

normalizeDeclarations : Node Declaration -> (Normalization.State, List (Node Declaration)) -> (Normalization.State, List (Node Declaration))
normalizeDeclarations original (normalizer, normalizedDeclarations) =
    let
        normalizedDeclaration = 
            Node.map
                (normalizeDeclaration normalizer)
                original
        normalizer1 = Node.value normalizedDeclaration |> Tuple.first
    in
        ( normalizer1
        , Node.map Tuple.second normalizedDeclaration :: normalizedDeclarations
        )


normalizeDeclaration : Normalization.State -> Declaration -> (Normalization.State, Declaration)
normalizeDeclaration normalizer declaration =
        case declaration of
            Declaration.AliasDeclaration typeAlias ->
                let
                    normalizedTypeAlias = normalizeTypeAlias normalizer typeAlias
                in
                    ( Tuple.first normalizedTypeAlias
                    , Declaration.AliasDeclaration (Tuple.second normalizedTypeAlias)
                    )

            _ ->
                (normalizer, declaration)


normalizeTypeAlias : Normalization.State -> TypeAlias -> (Normalization.State, TypeAlias)
normalizeTypeAlias normalizer original =
    let
        normalizedName =
            Node.map 
            (Normalization.normalize normalizer) 
            original.name
        typeAlias = 
            TypeAlias
                original.documentation
                (Node.map Tuple.second normalizedName)
                original.generics
                original.typeAnnotation      
    in
        ( Node.value normalizedName |> Tuple.first
        , typeAlias)
