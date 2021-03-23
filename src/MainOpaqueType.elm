port module Main exposing (main)

import Elm.Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Writer exposing (write, writeFile)
import IdentifierMapping as IdentifierMapping exposing (WithIdentifierMapping)
import List
import Platform exposing (Program)



-- This solution uses the IdentifierMapping module, which has an opaque type, and
-- hides the implementation details of how it creates unique id's, which is a good
-- thing.
-- As long as it is used correctly the types will always be unique, and no calling
-- code is able to change the auto incrementing id or anything like that.
-- However, you can (and indeed have to) copy the WithIdentifierMapping from one
-- value to another, in which case you can easily end up using an older version of
-- the it, so it is still possible, and arguably easier, to cause trouble
-- The 'IdentifierMapping.map (\_ -> typeAlias) declarationWithIdentifierMapping'
-- type code is what does this copying, and you would probably create a function
-- in the module for that if continuing with this approach


normaliseElmFile : WithIdentifierMapping File -> WithIdentifierMapping File
normaliseElmFile fileWithIdentifierMapping =
    let
        elmFile =
            IdentifierMapping.value fileWithIdentifierMapping

        normalizedDeclarationsWithIdentifierMapping =
            List.foldl normaliseDeclarations (IdentifierMapping.initialize []) elmFile.declarations
    in
    IdentifierMapping.map
        (\normalizedDeclarations ->
            File
                elmFile.moduleDefinition
                elmFile.imports
                normalizedDeclarations
                []
        )
        normalizedDeclarationsWithIdentifierMapping


normaliseDeclarations : Node Declaration -> WithIdentifierMapping (List (Node Declaration)) -> WithIdentifierMapping (List (Node Declaration))
normaliseDeclarations unNormalizedDeclaration normalisedDeclarationsWithIdentifierMapping =
    let
        normalisedDeclarations =
            IdentifierMapping.value normalisedDeclarationsWithIdentifierMapping

        unNormalizedNodeDeclarationWithIdentifierMapping =
            IdentifierMapping.map (\_ -> unNormalizedDeclaration) normalisedDeclarationsWithIdentifierMapping

        range =
            IdentifierMapping.value unNormalizedNodeDeclarationWithIdentifierMapping |> Node.range

        unNormalizedDeclarationWithIdentifierMapping =
            IdentifierMapping.map Node.value unNormalizedNodeDeclarationWithIdentifierMapping

        normalizedDeclarationWithIdentifierMapping =
            normaliseDeclaration unNormalizedDeclarationWithIdentifierMapping

        normalizedNodeDeclaration =
            IdentifierMapping.map (Node.Node range) normalizedDeclarationWithIdentifierMapping
    in
    IdentifierMapping.map
        (\normalizedDeclaration -> normalizedDeclaration :: normalisedDeclarations)
        normalizedNodeDeclaration


normaliseDeclaration : WithIdentifierMapping Declaration -> WithIdentifierMapping Declaration
normaliseDeclaration declarationWithIdentifierMapping =
    let
        declaration =
            IdentifierMapping.value declarationWithIdentifierMapping
    in
    case declaration of
        Declaration.AliasDeclaration typeAlias ->
            normaliseTypeAlias (IdentifierMapping.map (\_ -> typeAlias) declarationWithIdentifierMapping)

        _ ->
            declarationWithIdentifierMapping


normaliseTypeAlias : WithIdentifierMapping TypeAlias -> WithIdentifierMapping Declaration
normaliseTypeAlias typeAliasWithIdentifierMapping =
    let
        typeAlias =
            IdentifierMapping.value typeAliasWithIdentifierMapping

        nameWithIdentifierMapping =
            IdentifierMapping.map (\t -> t.name) typeAliasWithIdentifierMapping |> normaliseNodeString
    in
    IdentifierMapping.map
        (\_ ->
            Declaration.AliasDeclaration
                (TypeAlias
                    typeAlias.documentation
                    (IdentifierMapping.value nameWithIdentifierMapping)
                    typeAlias.generics
                    typeAlias.typeAnnotation
                 -- maybe normalise, maybe remove
                )
        )
        nameWithIdentifierMapping


normaliseNodeString : WithIdentifierMapping (Node String) -> WithIdentifierMapping (Node String)
normaliseNodeString unNormalizedNodeStringWithNormalizer =
    let
        nodeString =
            IdentifierMapping.value unNormalizedNodeStringWithNormalizer

        range =
            Node.range nodeString
    in
    IdentifierMapping.normalise Node.value (\normalized -> Node.Node range normalized) unNormalizedNodeStringWithNormalizer


type alias InputType =
    String


type alias OutputType =
    String


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
        Input input ->
            ( model, put (transform input) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input


transform : InputType -> OutputType
transform unNormalised =
    case Elm.Parser.parse unNormalised of
        Err error ->
            "Failed: " ++ Debug.toString error

        Ok rawFile ->
            -- "Success: " ++ (Debug.toString (Elm.RawFile.encode v))
            process init rawFile
                |> IdentifierMapping.initialize
                |> normaliseElmFile
                |> writeResults



--|> writeFile
--|> write


writeResults : IdentifierMapping.WithIdentifierMapping File -> String
writeResults fileWithMapping =
    IdentifierMapping.value fileWithMapping
        |> writeFile
        |> write
        |> (++) (Debug.toString fileWithMapping)
