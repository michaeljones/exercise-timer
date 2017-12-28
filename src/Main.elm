module Main exposing (..)

import Html exposing (..)
import Navigation


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = NoOp
    | UrlChange Navigation.Location


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    {} ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UrlChange location ->
            model ! []


view : Model -> Html Msg
view model =
    div [] [ text "Hello world" ]
