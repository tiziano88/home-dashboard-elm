module Main exposing (..)

import Html
import Html.Events
import Http
import Json.Decode as JD
import Json.Encode as JE
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Scheme


type alias Model =
    { serverUrl : String
    , res : Maybe SyncResponse
    , devices : List Device
    , mdl : Material.Model
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
    , devices : List DeviceResponse
    }


type alias DeviceResponse =
    { id : String
    , name : Name
    , traits : List String
    , type_ : String
    , willReportState : Bool
    }


type alias Name =
    { name : String
    }


type alias ExecuteRequest =
    { requestId : String
    , inputs : List ExecuteRequestInput
    }


type alias ExecuteRequestInput =
    { intent : String
    , payload : ExecuteRequestPayload
    }


type alias ExecuteRequestPayload =
    { commands : List Command
    }


type alias Command =
    { devices : List DeviceRequest
    , execution : List Execution
    }


type alias DeviceRequest =
    { id : String
    }


type alias Execution =
    { command : String
    , params : Params
    }


type alias Params =
    { on : Bool
    }


type Device
    = Light { id : String, name : String }


toDevice : DeviceResponse -> Device
toDevice d =
    Light { id = d.id, name = d.name.name }


syncResponseDecoder : JD.Decoder SyncResponse
syncResponseDecoder =
    JD.map2 SyncResponse
        (JD.field "requestId" JD.string)
        (JD.field "payload" <|
            JD.map2 Payload
                (JD.field "agentUserId" JD.string)
                (JD.field "devices" <|
                    JD.list <|
                        JD.map5 DeviceResponse
                            (JD.field "id" JD.string)
                            (JD.field "name" <| JD.map Name (JD.field "name" JD.string))
                            (JD.field "traits" <| JD.list JD.string)
                            (JD.field "type" JD.string)
                            (JD.field "willReportState" JD.bool)
                )
        )


type Msg
    = Nop
    | Mdl (Material.Msg Msg)
    | Sync (Result Http.Error SyncResponse)
    | Exec ExecuteRequest


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


executeRequestEncoder : ExecuteRequest -> JE.Value
executeRequestEncoder req =
    JE.object
        [ ( "requestId", JE.string req.requestId )
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


onOffRequest : String -> Bool -> ExecuteRequest
onOffRequest deviceId onOff =
    { requestId = "11"
    , inputs =
        [ { intent = "action.devices.EXECUTE"
          , payload =
                { commands =
                    [ { devices =
                            [ { id = deviceId
                              }
                            ]
                      , execution =
                            [ { command = "action.devices.command.OnOff"
                              , params =
                                    { on = onOff
                                    }
                              }
                            ]
                      }
                    ]
                }
          }
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( { serverUrl = "http://localhost:1234/action"
      , res = Nothing
      , devices = []
      , mdl = Material.model
      }
    , Http.send Sync <| Http.post serverUrl (Http.jsonBody syncRequest) syncResponseDecoder
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Sync (Ok r) ->
            ( { model | res = Just r, devices = List.map toDevice r.payload.devices }, Cmd.none )

        Sync _ ->
            ( model, Cmd.none )

        Exec req ->
            ( model
            , Http.send Sync <| Http.post serverUrl (Http.jsonBody <| executeRequestEncoder req) syncResponseDecoder
            )

        Nop ->
            ( model, Cmd.none )


viewDevice : Model -> Device -> Html.Html Msg
viewDevice model d =
    case d of
        Light l ->
            Card.view
                [ css "width" "10em"
                , Elevation.e8
                ]
                [ Card.title []
                    [ Card.head [] [ Html.text l.name ]
                    ]
                , Card.actions []
                    [ Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Options.onClick <| Exec <| onOffRequest l.id True
                        ]
                        [ Html.text "on" ]
                    , Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Options.onClick <| Exec <| onOffRequest l.id False
                        ]
                        [ Html.text "off" ]
                    ]
                ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        ([ Html.text "test"
           --, Button.render a
         ]
            ++ List.map (viewDevice model) model.devices
        )
        |> Material.Scheme.top
