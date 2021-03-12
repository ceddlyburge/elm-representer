port module Main exposing (main)

import Platform exposing (Program)
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
import Dict as Dict
import Elm.Writer exposing (writeFile)

-- This solution uses tuples to pass back the normalizer along with the normalized
-- I'm generally not too keen on tuples, but the functions do need to return two
-- things, and there is no obvious Type solution
-- Each type has a short lifetime as well, although there are quite a few of them, 
-- none of them are passed beyond one function boundary 
-- Its quite a small amount of code, and not much hacky code
-- It doesn't use an opaque type yet, so still an unsafe solution, but maybe that
-- can be improved in a future iteration
type alias Normalizer =
    { mapping: (Dict.Dict String String)
    , uniqueInt: Int
    }


normaliseElmFile : Normalizer -> File -> (Normalizer, File)
normaliseElmFile normalizer original = 
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

normalizeDeclarations : Node Declaration -> (Normalizer, List (Node Declaration)) -> (Normalizer, List (Node Declaration))
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


normalizeDeclaration : Normalizer -> Declaration -> (Normalizer, Declaration)
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


normalizeTypeAlias : Normalizer -> TypeAlias -> (Normalizer, TypeAlias)
normalizeTypeAlias normalizer original =
    let
        normalizedName =
            Node.map 
            (normalizeString normalizer) 
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
        
normalizeString : Normalizer -> String -> (Normalizer, String)
normalizeString normalizer original =
    let
        normalized = "IDENTIFIER_" ++ String.fromInt normalizer.uniqueInt
        nextNormalizer = 
           Normalizer
                (Dict.insert original normalized normalizer.mapping)
                (normalizer.uniqueInt + 1)
    in
        (nextNormalizer, normalized)

type alias InputType = String
type alias OutputType = String

port get : (InputType -> msg) -> Sub msg

port put : OutputType -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init2
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    ()


type Msg
    = Input String


type alias Flags =
    ()


init2 : Flags -> ( Model, Cmd Msg )
init2 _ =
    ( (), Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input -> ( model, put (transform input))


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input


transform : InputType -> OutputType
transform unNormalised =
    case Elm.Parser.parse unNormalised of
        Err error ->
            "Failed: " ++ Debug.toString error
        Ok rawFile ->
            normaliseElmFile 
                (Normalizer Dict.empty 0)
                (process init rawFile) 
            |> writeResults
    
writeResults: (Normalizer, File) -> String
writeResults (normalizer, file) =
        file
        |> writeFile 
        |> write
        |> (++) (Debug.toString normalizer)

