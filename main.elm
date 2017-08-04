module Main exposing (..)

import Html
import Http
import Json.Decode as JD
import Json.Encode


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
    Json.Encode.object
        [ ( "requestId", Json.Encode.string "xxx" )
        , ( "inputs"
          , Json.Encode.list
                [ Json.Encode.object
                    [ ( "intent", Json.Encode.string "action.devices.SYNC" )
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

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.text "test"
