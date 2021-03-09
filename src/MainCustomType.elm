port module Main exposing (main)

import Platform exposing (Program)
import Elm.Parser
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Writer exposing (writeFile, write)
import Elm.Processing exposing (init, process)
import List
import Dict as Dict
import Html.Attributes exposing (name)
import Elm.Writer exposing (writeFile)

-- This solution uses fully exposed Custom Types. They are currently in this 
-- module, but could be moved to another one easily enough.
-- The code can, and has to, get at the details of the normaliser / mapper,
-- which opens up opportunities to get it wrong (by forgetting to update
-- the uniqueInt for example)
type IdentifierMapping = 
    IdentifierMapping (Dict.Dict String String) Int


type WithIdentifierMapping a = 
    WithIdentifierMapping IdentifierMapping a

withoutIdentifierMapping : WithIdentifierMapping a -> a
withoutIdentifierMapping (WithIdentifierMapping _ value) = 
    value

getIdentifierMapping : WithIdentifierMapping a -> IdentifierMapping
getIdentifierMapping (WithIdentifierMapping identifierMapping _) = 
    identifierMapping


normaliseElmFile: WithIdentifierMapping File -> WithIdentifierMapping File
normaliseElmFile (WithIdentifierMapping identifierMapping elmFile) = 
    let
        (WithIdentifierMapping nextIdentifierMapping declarations) = List.foldl normaliseDeclarations (WithIdentifierMapping identifierMapping []) elmFile.declarations
    in
        WithIdentifierMapping
            nextIdentifierMapping
            (File
                elmFile.moduleDefinition
                elmFile.imports
                declarations
                []
            )

normaliseDeclarations : Node Declaration -> WithIdentifierMapping (List (Node Declaration)) -> WithIdentifierMapping (List (Node Declaration))
normaliseDeclarations declaration (WithIdentifierMapping identifierMapping normalisedDeclarations) =
    let
        normalisedDeclarationAndMapping = Node.map (normaliseDeclaration identifierMapping) declaration
        normalisedDeclaration = Node.map withoutIdentifierMapping normalisedDeclarationAndMapping
        nextIdentifierMapping = Node.map getIdentifierMapping normalisedDeclarationAndMapping |> Node.value
    in
        WithIdentifierMapping
            nextIdentifierMapping
            (normalisedDeclaration :: normalisedDeclarations)


normaliseDeclaration : IdentifierMapping ->  Declaration -> WithIdentifierMapping Declaration
normaliseDeclaration identifierMapping decl =
        case decl of
            Declaration.AliasDeclaration typeAlias ->
                normaliseTypeAlias <| WithIdentifierMapping identifierMapping typeAlias

            _ ->
                WithIdentifierMapping identifierMapping decl


normaliseTypeAlias: WithIdentifierMapping TypeAlias -> WithIdentifierMapping Declaration
normaliseTypeAlias (WithIdentifierMapping identifierMapping typeAlias) =
    let
        (WithIdentifierMapping identifierMapping1 name) = normaliseString (WithIdentifierMapping identifierMapping typeAlias.name)
    in
        WithIdentifierMapping
            identifierMapping1
            (Declaration.AliasDeclaration
                (TypeAlias
                    typeAlias.documentation
                    name
                    typeAlias.generics
                    typeAlias.typeAnnotation -- maybe normalise, maybe remove
                )
            )

normaliseString: WithIdentifierMapping (Node String) -> WithIdentifierMapping (Node String)
normaliseString (WithIdentifierMapping (IdentifierMapping mapping uniqueInt) unNormalised) =
    let
        normalised = "IDENTIFIER_" ++ String.fromInt uniqueInt
        nextIdentifierMapping = Dict.insert (Node.value unNormalised) normalised mapping
    in
        WithIdentifierMapping
            (IdentifierMapping nextIdentifierMapping (uniqueInt + 1))
            (Node.map (\_ -> normalised) unNormalised)


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
            process init rawFile
            |> WithIdentifierMapping (IdentifierMapping Dict.empty 0)
            |> normaliseElmFile 
            |> writeResults
    
writeResults: WithIdentifierMapping File -> String
writeResults (WithIdentifierMapping identifierMapping file) =
    writeFile file
    |> write
    |> (++) (Debug.toString identifierMapping)

