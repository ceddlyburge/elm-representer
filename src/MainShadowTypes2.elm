port module Main exposing (main)

import Dict as Dict
import Elm.Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Expression exposing (Expression, Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Writer exposing (write, writeFile)
import List
import Platform exposing (Program)



-- This solution uses a shadow type structure to that used in Elm Syntax. Having to create
-- all the shadow types is a bit of an annoyance, as is mapping back from this shadow
-- structure to the original strucutre, but otherwise I think the solution is a
-- lot cleaner than the others.
-- It does have quite a lot more code though, but I think its all more obvious and simple
-- goals
-- use an opaque type
-- sort out the (getNormalizer (Node.value normalizedDeclaration)) problem
--  the node.value bit might be intractable though
-- reduce code
--  maybe the mapping back
-- fix very long type definition in normalizeDeclarations


type alias Normalizer =
    { mapping : Dict.Dict String String
    , uniqueInt : Int
    }


type alias NormalizedFile =
    { moduleDefinition : Node Module
    , imports : List (Node Import)
    , declarations : List (Node (History Declaration NormalizedDeclaration))
    , comments : List (Node Comment)
    }


type NormalizedDeclaration
    = FunctionDeclaration Function
    | AliasDeclaration (History TypeAlias NormalizedTypeAlias)
    | CustomTypeDeclaration Type
    | PortDeclaration Signature
    | InfixDeclaration Infix
    | Destructuring (Node Pattern) (Node Expression)


type alias NormalizedTypeAlias =
    { documentation : Maybe (Node Documentation)
    , name : Node (History String String)
    , generics : List (Node String) -- do after proof of concept
    , typeAnnotation : Node TypeAnnotation
    }



-- doesn't necessarily need to keep the input, but maybe handy for debugging and suchlike


type History a b
    = History a Normalizer b



-- | Initial a Normalizer b -- would this be a useful thing?
-- | History a (History c d) b -- would this be a useful thing?


normalizeElmFile : Normalizer -> File -> History File NormalizedFile
normalizeElmFile normalizer original =
    let
        normalizedDeclarations =
            List.foldl
                normalizeDeclarations
                ( normalizer, [] )
                original.declarations

        normalizedFile =
            NormalizedFile
                original.moduleDefinition
                original.imports
                (Tuple.second normalizedDeclarations)
                []
    in
    History
        original
        (Tuple.first normalizedDeclarations)
        normalizedFile



-- this type annotation is an annoyance
-- the tuple is kind of unecessary, as the normalizer is also in the items of the list,
-- but its handy for the first iteration, when there is nothing in the list. I think any
-- other way of doing it would end up worse.


normalizeDeclarations : Node Declaration -> ( Normalizer, List (Node (History Declaration NormalizedDeclaration)) ) -> ( Normalizer, List (Node (History Declaration NormalizedDeclaration)) )
normalizeDeclarations original ( normalizer, normalizedDeclarations ) =
    let
        normalizedDeclaration =
            Node.map
                (normalizeDeclaration normalizer)
                original

        normalizer1 =
            getNormalizer (Node.value normalizedDeclaration)

        -- this is an annoyance
    in
    ( normalizer1
    , normalizedDeclaration :: normalizedDeclarations
    )


normalizeDeclaration : Normalizer -> Declaration -> History Declaration NormalizedDeclaration
normalizeDeclaration normalizer declaration =
    case declaration of
        Declaration.AliasDeclaration typeAlias ->
            let
                normalizedTypeAlias =
                    normalizeTypeAlias typeAlias normalizer
            in
            History
                (Declaration.AliasDeclaration typeAlias)
                (getNormalizer normalizedTypeAlias)
                -- this is an annoyance
                (AliasDeclaration normalizedTypeAlias)

        Declaration.FunctionDeclaration function ->
            History
                declaration
                normalizer
                (FunctionDeclaration function)

        Declaration.CustomTypeDeclaration typeDeclaration ->
            History
                declaration
                normalizer
                (CustomTypeDeclaration typeDeclaration)

        Declaration.PortDeclaration p ->
            History
                declaration
                normalizer
                (PortDeclaration p)

        Declaration.InfixDeclaration inf ->
            History
                declaration
                normalizer
                (InfixDeclaration inf)

        Declaration.Destructuring x y ->
            History
                declaration
                normalizer
                (Destructuring x y)


normalizeTypeAlias : TypeAlias -> Normalizer -> History TypeAlias NormalizedTypeAlias
normalizeTypeAlias original normalizer =
    let
        normalizedName =
            Node.map
                (normalizeString normalizer)
                original.name

        normalized =
            NormalizedTypeAlias
                original.documentation
                normalizedName
                original.generics
                original.typeAnnotation

        typeAlias =
            TypeAlias
                original.documentation
                (toElmString normalizedName)
                original.generics
                original.typeAnnotation
    in
    -- this isn't any better is it?
    -- createHistory
    --     original
    --     (Node.value normalizedName)
    --     normalized
    History
        original
        (getNormalizer (Node.value normalizedName))
        -- this is an annoyance
        normalized


getNormalizer : History a b -> Normalizer
getNormalizer (History _ normalizer _) =
    normalizer



-- createHistory : a -> (History _ _ ) -> b -> History a b
-- createHistory original previousHistory normalized =
--     History
--         original
--         (getNormalizer previousHistory)
--         normalized


normalizeString : Normalizer -> String -> History String String
normalizeString normalizer original =
    let
        normalized =
            "IDENTIFIER_" ++ String.fromInt normalizer.uniqueInt

        nextNormalizer =
            Normalizer
                (Dict.insert original normalized normalizer.mapping)
                (normalizer.uniqueInt + 1)
    in
    History
        original
        nextNormalizer
        normalized


toElmFile : History File NormalizedFile -> File
toElmFile (History _ _ normalized) =
    File
        normalized.moduleDefinition
        normalized.imports
        (List.map (Node.map toDeclaration) normalized.declarations)
        normalized.comments


toDeclaration : History Declaration NormalizedDeclaration -> Declaration
toDeclaration (History _ _ normalized) =
    case normalized of
        AliasDeclaration normalizedAliasDeclaration ->
            Declaration.AliasDeclaration (toTypeAlias normalizedAliasDeclaration)

        FunctionDeclaration function ->
            Declaration.FunctionDeclaration function

        CustomTypeDeclaration typeDeclaration ->
            Declaration.CustomTypeDeclaration typeDeclaration

        PortDeclaration p ->
            Declaration.PortDeclaration p

        InfixDeclaration inf ->
            Declaration.InfixDeclaration inf

        Destructuring x y ->
            Declaration.Destructuring x y


toTypeAlias : History a NormalizedTypeAlias -> TypeAlias
toTypeAlias (History _ _ normalized) =
    TypeAlias
        normalized.documentation
        (Node.map toElmString normalized.name)
        normalized.generics
        normalized.typeAnnotation


toElmString : History a String -> String
toElmString (History _ _ normalized) =
    normalized


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
            normalizeElmFile
                (Normalizer Dict.empty 0)
                (process init rawFile)
                |> writeResults


writeResults : History File NormalizedFile -> String
writeResults normalizedFile =
    let
        (History _ normalizer _) =
            normalizedFile
    in
    normalizedFile
        |> toElmFile
        |> writeFile
        |> write
        |> (++) (Debug.toString normalizer)
