module Main exposing (..)

import Html
import Http
import Json.Decode
import Json.Encode


type alias Model =
    { serverUrl : String
    , devices : List Device
    }


type alias ActionRequest =
    { requestId : String
    , inputs : List ActionRequestInput
    }


type alias ActionRequestInput =
    { intent : String
    , payload : Payload
    }


type alias Payload =
    {}


type Msg
    = Nop


type Device
    = Light


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


serverUrl =
    "http://127.0.0.1:1234/action"


syncRequest =
    Json.Encode.object
        [ ( "request_id", Json.Encode.string "xxx" )
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
      , devices = []
      }
    , Http.send (always Nop) <| Http.post serverUrl (Http.jsonBody syncRequest) Json.Decode.string
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.text "test"
