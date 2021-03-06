port module Main exposing (main)

import Dict exposing (Dict)
import Json.Encode
import NormalizeElmCode
import Platform exposing (Program)


type alias InputType =
    String


type alias OutputType =
    ( String, String )


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
    NormalizeElmCode.normalize unNormalised
        |> writeResults


writeResults : Result String ( Dict String String, String ) -> OutputType
writeResults results =
    case results of
        Ok ( identifierMapping, normalizedElmCode ) ->
            --++ Debug.toString identifierMapping
            ( normalizedElmCode
            , Json.Encode.encode 1 (Json.Encode.dict identity Json.Encode.string identifierMapping)
            )

        Err message ->
            ( message, "{}" )
