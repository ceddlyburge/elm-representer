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
import State as State

-- This solution attempts to use the State monad, but it got too complicated for
-- me to comprehend. 
-- I'm sure its possible, but it is maybe too complex to be worthwhile.
-- Another alternative is that I don't understand the state monad well enough
-- I should maybe do a much more cimple example instead so I can get my head round it
-- just normalize a few things of the same type in a row
-- then maybe create a type alias with it
-- then maybe thing about how to use with the Node type

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
        state = State.state (Node.value original.name)
        
        state2 = State.map 
            (\name ->
                TypeAlias
                    original.documentation
                    (Node.map (\_ -> normali)
                    original.generics
                    original.typeAnnotation      
            )
            state

        (typeAlias, nextNormalizer) -> State.run normalizer state2
    in
        (nextNormalizer, typeAlias)

-- normalizeString : State Normalizer String -> State Normalizer String
-- normalizeString state =
--     let
--         -- it seems weird that we have to map / modify the state and value
--         -- separately. I thought the whole point of the state monad was to handle
--         -- the composisiont of functions like the original normalizeString
--         -- At the moment, this is just making the code worse
--         -- We could create a function for this though. mapAndModify or something
--         normalized = "IDENTIFIER_" ++ String.fromInt normalizer.uniqueInt
--         nextState = State.modify 
--             (\nextNormalizer -> 
--                 Normalizer
--                         (Dict.insert original normalized normalizer.mapping)
--                         (normalizer.uniqueInt + 1)
--             )
--             state
        
--     in
--         State.map (\_ -> normalized) nextState

mapAndModify : (s -> s) -> (a -> b) -> State s a -> State s b
mapAndModify mapState mapValue state = 
    state
    |> State.map mapValue
    |> State.modify mapState

normalizeString : String -> Normalizer -> (String, Normalizer)
normalizeString original normalizer =
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

