module Main exposing (..)

import Html
import Html.Events
import Http
import Json.Decode as JD
import Json.Encode as JE


type alias Model =
    { serverUrl : String
    , res : Maybe SyncResponse
    }


type alias ActionRequest =
    { requestId : String
    , inputs : List ActionRequestInput
    }


type alias ActionRequestInput =
    { intent : String
    , payload : Payload
    }


type alias SyncResponse =
    { requestId : String
    , payload : Payload
    }


type alias Payload =
    { agentUserId : String
    , devices : List Device
    }


type alias Device =
    { id : String
    , name : Name
    , traits : List String
    , type_ : String
    , willReportState : Bool
    }


type alias Name =
    { name : String
    }


syncResponseDecoder : JD.Decoder SyncResponse
syncResponseDecoder =
    JD.map2 SyncResponse
        (JD.field "requestId" JD.string)
        (JD.field "payload" <|
            JD.map2 Payload
                (JD.field "agentUserId" JD.string)
                (JD.field "devices" <|
                    JD.list <|
                        JD.map5 Device
                            (JD.field "id" JD.string)
                            (JD.field "name" <| JD.map Name (JD.field "name" JD.string))
                            (JD.field "traits" <| JD.list JD.string)
                            (JD.field "type" JD.string)
                            (JD.field "willReportState" JD.bool)
                )
        )


type Msg
    = Nop
    | Sync (Result Http.Error SyncResponse)
    | Exec


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


serverUrl =
    "http://192.168.0.21:1234/action"


syncRequest =
    JE.object
        [ ( "requestId", JE.string "xxx" )
        , ( "inputs"
          , JE.list
                [ JE.object
                    [ ( "intent", JE.string "action.devices.SYNC" )
                    ]
                ]
          )
        ]


executeRequest =
    JE.object
        [ ( "requestId", JE.string "xxx" )
        , ( "inputs"
          , JE.list
                [ JE.object
                    [ ( "intent", JE.string "action.devices.EXECUTE" )
                    , ( "payload"
                      , JE.object
                            [ ( "commands"
                              , JE.list
                                    [ JE.object
                                        [ ( "devices"
                                          , JE.list
                                                [ JE.object
                                                    [ ( "id", JE.string "11" )
                                                    ]
                                                ]
                                          )
                                        , ( "execution"
                                          , JE.list
                                                [ JE.object
                                                    [ ( "command", JE.string "action.devices.command.OnOff" )
                                                    , ( "params"
                                                      , JE.object
                                                            [ ( "on", JE.bool True )
                                                            ]
                                                      )
                                                    ]
                                                ]
                                          )
                                        ]
                                    ]
                              )
                            ]
                      )
                    ]
                ]
          )
        ]


init : ( Model, Cmd Msg )
init =
    ( { serverUrl = "http://localhost:1234/action"
      , res = Nothing
      }
    , Http.send Sync <| Http.post serverUrl (Http.jsonBody syncRequest) syncResponseDecoder
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Sync (Ok r) ->
            ( { model | res = Just r }, Cmd.none )

        Sync _ ->
            ( model, Cmd.none )

        Exec ->
            ( model
            , Http.send Sync <| Http.post serverUrl (Http.jsonBody executeRequest) syncResponseDecoder
            )

        Nop ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text "test"
        , Html.button [ Html.Events.onClick Exec ] [ Html.text "execute" ]
        ]
