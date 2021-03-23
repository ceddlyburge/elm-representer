port module Main exposing (main)

import Dict as Dict
import Elm.Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Writer exposing (write, writeFile)
import List
import Platform exposing (Program)



-- This solution uses extensible records with a type alias for the normaliser / mapping
-- Each Elm Syntax Type basically requires its own type alias for this to work, which
-- feels like a pain. Maybe creating a copy of Elm Syntax Type structure here is a good
-- idea though, and the pain of it would pay for itself. It might make the type system
-- easier to work with
-- This could become an opaque type possibly, might be a good half way house type thing


type alias IdentifierMapping =
    { mapping : Dict.Dict String String
    , uniqueInt : Int
    }


type alias WithIdentifierMapping syntax =
    { syntax
        | identifierMapping : IdentifierMapping
    }


normaliseElmFile : WithIdentifierMapping { file : File } -> WithIdentifierMapping { file : File }
normaliseElmFile fileWithIdentifierMapping =
    let
        declarationsWithIdentifierMapping =
            List.foldl
                normaliseDeclarations
                { identifierMapping = fileWithIdentifierMapping.identifierMapping
                , declarations = []
                }
                -- could have a function to create the WithIdentifierMappping type more concisely
                fileWithIdentifierMapping.file.declarations
    in
    { identifierMapping = declarationsWithIdentifierMapping.identifierMapping
    , file =
        File
            fileWithIdentifierMapping.file.moduleDefinition
            fileWithIdentifierMapping.file.imports
            declarationsWithIdentifierMapping.declarations
            []
    }


normaliseDeclarations : Node Declaration -> WithIdentifierMapping { declarations : List (Node Declaration) } -> WithIdentifierMapping { declarations : List (Node Declaration) }
normaliseDeclarations nodeDeclaration normalisedDeclarationsWithIdentifierMapping =
    let
        declaration =
            Node.value nodeDeclaration

        normalizedDeclarationWithIdentifierMapping =
            normaliseDeclaration normalisedDeclarationsWithIdentifierMapping.identifierMapping declaration

        normalizedNodeDeclaration =
            Node.map (\_ -> normalizedDeclarationWithIdentifierMapping.declaration) nodeDeclaration
    in
    { identifierMapping = normalizedDeclarationWithIdentifierMapping.identifierMapping
    , declarations = normalizedNodeDeclaration :: normalisedDeclarationsWithIdentifierMapping.declarations
    }


normaliseDeclaration : IdentifierMapping -> Declaration -> WithIdentifierMapping { declaration : Declaration }
normaliseDeclaration identifierMapping decl =
    case decl of
        Declaration.AliasDeclaration typeAlias ->
            normaliseTypeAlias
                { identifierMapping = identifierMapping
                , typeAlias = typeAlias
                }

        _ ->
            { identifierMapping = identifierMapping
            , declaration = decl
            }


normaliseTypeAlias : WithIdentifierMapping { typeAlias : TypeAlias } -> WithIdentifierMapping { declaration : Declaration }
normaliseTypeAlias unNormalizedTypeAliasWithIdentifierMapping =
    let
        typeAlias =
            unNormalizedTypeAliasWithIdentifierMapping.typeAlias

        normalizedNameWithIdentifierMapping =
            normaliseString
                { identifierMapping = unNormalizedTypeAliasWithIdentifierMapping.identifierMapping
                , nodeString = typeAlias.name
                }
    in
    { identifierMapping = normalizedNameWithIdentifierMapping.identifierMapping
    , declaration =
        Declaration.AliasDeclaration
            (TypeAlias
                typeAlias.documentation
                normalizedNameWithIdentifierMapping.nodeString
                typeAlias.generics
                -- want to normalise this, removed to create simple case to compare approaches
                typeAlias.typeAnnotation
             -- maybe normalise, maybe remove
            )
    }


normaliseString : WithIdentifierMapping { nodeString : Node String } -> WithIdentifierMapping { nodeString : Node String }
normaliseString unNormalisedWithIdentifierMapping =
    let
        identifierMapping =
            unNormalisedWithIdentifierMapping.identifierMapping

        unNormalisedNodeString =
            unNormalisedWithIdentifierMapping.nodeString

        normalisedString =
            "IDENTIFIER_" ++ String.fromInt identifierMapping.uniqueInt

        nextIdentifierMapping =
            Dict.insert (Node.value unNormalisedNodeString) normalisedString identifierMapping.mapping
    in
    { identifierMapping = IdentifierMapping nextIdentifierMapping (identifierMapping.uniqueInt + 1)
    , nodeString = Node.map (\_ -> normalisedString) unNormalisedNodeString
    }


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
            { identifierMapping = IdentifierMapping Dict.empty 0
            , file = process init rawFile
            }
                |> normaliseElmFile
                |> writeResults


writeResults : WithIdentifierMapping { file : File } -> String
writeResults fileWithIdentifierMapping =
    writeFile fileWithIdentifierMapping.file
        |> write
        |> (++) (Debug.toString fileWithIdentifierMapping.identifierMapping)
