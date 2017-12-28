module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Navigation
import Process
import Task
import Time


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


type Mode
    = CreateMode
    | RunMode Block


type alias Period t =
    { minutes : t
    , seconds : t
    }


periodInit : Period String
periodInit =
    { minutes = "0"
    , seconds = "0"
    }


setMinutes : Period t -> t -> Period t
setMinutes period minutes =
    { period | minutes = minutes }


setSeconds : Period t -> t -> Period t
setSeconds period seconds =
    { period | seconds = seconds }


type ActiveSection
    = WorkSection
    | RestSection


type alias Block =
    { current : ActiveSection
    , work : Period Int
    , rest : Period Int
    , minutes : Int
    , seconds : Int
    , roundsRemaining : Int
    , startTime : Time.Time
    }


type alias Model =
    { mode : Mode
    , work : Period String
    , rest : Period String
    , rounds : String
    }


type Msg
    = NoOp
    | UrlChange Navigation.Location
    | SetWorkMinutes String
    | SetWorkSeconds String
    | SetRestMinutes String
    | SetRestSeconds String
    | SetRounds String
    | StartRun
    | StartTime Block Time.Time
    | Tick Time.Time
    | Tock


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    { mode = CreateMode
    , work = periodInit
    , rest = periodInit
    , rounds = "1"
    }
        ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every Time.second Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UrlChange location ->
            model ! []

        SetWorkMinutes string ->
            { model | work = setMinutes model.work string } ! []

        SetWorkSeconds string ->
            { model | work = setSeconds model.work string } ! []

        SetRestMinutes string ->
            { model | rest = setMinutes model.rest string } ! []

        SetRestSeconds string ->
            { model | rest = setSeconds model.rest string } ! []

        SetRounds string ->
            { model | rounds = string } ! []

        StartRun ->
            let
                workMins =
                    String.toInt model.work.minutes

                workSecs =
                    String.toInt model.work.seconds

                restMins =
                    String.toInt model.rest.minutes

                restSecs =
                    String.toInt model.rest.seconds

                rounds =
                    String.toInt model.rounds
            in
            case Result.map5 (,,,,) workMins workSecs restMins restSecs rounds of
                Ok ( wm, ws, rm, rs, rr ) ->
                    let
                        block =
                            { current = WorkSection
                            , work = { minutes = wm, seconds = ws }
                            , rest = { minutes = rm, seconds = rs }
                            , minutes = wm
                            , seconds = ws
                            , roundsRemaining = rr
                            , startTime = 0
                            }
                    in
                    model
                        ! [ Task.perform (StartTime block) Time.now
                          ]

                Err err ->
                    model ! []

        StartTime block time ->
            { model | mode = RunMode { block | startTime = time } } ! []

        Tick time ->
            case model.mode of
                RunMode block ->
                    let
                        diff =
                            round (time - block.startTime) % 1000

                        task =
                            Process.sleep (toFloat diff)
                    in
                    model
                        ! [ Task.perform (\_ -> Tock) task
                          ]

                CreateMode ->
                    model ! []

        Tock ->
            case model.mode of
                CreateMode ->
                    model ! []

                RunMode ({ minutes, seconds } as block) ->
                    case ( minutes, seconds ) of
                        ( 0, 0 ) ->
                            case ( block.current, block.roundsRemaining ) of
                                ( WorkSection, 0 ) ->
                                    { model | mode = CreateMode } ! []

                                ( WorkSection, _ ) ->
                                    { model
                                        | mode =
                                            RunMode
                                                { block
                                                    | current = RestSection
                                                    , minutes = block.rest.minutes
                                                    , seconds = block.rest.seconds
                                                }
                                    }
                                        ! []

                                ( RestSection, _ ) ->
                                    { model
                                        | mode =
                                            RunMode
                                                { block
                                                    | current = WorkSection
                                                    , minutes = block.work.minutes
                                                    , seconds = block.work.seconds
                                                    , roundsRemaining = block.roundsRemaining - 1
                                                }
                                    }
                                        ! []

                        ( minutes, 0 ) ->
                            { model
                                | mode = RunMode { block | minutes = minutes - 1, seconds = 59 }
                            }
                                ! []

                        ( minutes, seconds ) ->
                            { model
                                | mode = RunMode { block | seconds = seconds - 1 }
                            }
                                ! []


view : Model -> Html Msg
view model =
    case model.mode of
        CreateMode ->
            div [ id "container" ]
                [ div []
                    [ input [ type_ "number", onInput SetWorkMinutes, value model.work.minutes ] []
                    , input [ type_ "number", onInput SetWorkSeconds, value model.work.seconds ] []
                    ]
                , div []
                    [ input [ type_ "number", onInput SetRestMinutes, value model.rest.minutes ] []
                    , input [ type_ "number", onInput SetRestSeconds, value model.rest.seconds ] []
                    ]
                , div []
                    [ input [ type_ "number", onInput SetRounds, value model.rounds ] []
                    ]
                , div []
                    [ button [ onClick StartRun ] [ text "Start" ]
                    ]
                ]

        RunMode block ->
            let
                className =
                    case block.current of
                        WorkSection ->
                            "work"

                        RestSection ->
                            "rest"
            in
            div [ id "container", class className ]
                [ div [ class "counter" ]
                    [ text (String.padLeft 2 '0' <| toString block.minutes)
                    , text ":"
                    , text (String.padLeft 2 '0' <| toString block.seconds)
                    ]
                ]
