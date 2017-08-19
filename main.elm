module Main exposing (..)

import Bitwise
import Color
import Dict
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
import Material.Slider as Slider


type alias Model =
    { serverUrl : String
    , res : Maybe SyncResponse
    , devices : Dict.Dict DeviceId Device
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
    { id : DeviceId
    }


type alias Execution =
    { command : String
    , params : Params
    }


type alias Params =
    { on : Maybe Bool
    , spectrumRGB : Maybe Int
    }


type Device
    = Light
        { id : DeviceId
        , name : String
        , hue : Float
        }


toDevice : DeviceResponse -> Device
toDevice d =
    Light
        { id = d.id
        , name = d.name.name
        , hue = 0
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
                        JD.map5 DeviceResponse
                            (JD.field "id" JD.string)
                            (JD.field "name" <| JD.map Name (JD.field "name" JD.string))
                            (JD.field "traits" <| JD.list JD.string)
                            (JD.field "type" JD.string)
                            (JD.field "willReportState" JD.bool)
                )
        )


type alias DeviceId =
    String


type Msg
    = Nop
    | Mdl (Material.Msg Msg)
    | Sync (Result Http.Error SyncResponse)
    | Exec ExecuteRequest
    | SetHue DeviceId Float


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
          , req.inputs
                |> List.map
                    (\input ->
                        JE.object
                            [ ( "intent", JE.string input.intent )
                            , ( "payload"
                              , JE.object
                                    [ ( "commands"
                                      , input.payload.commands
                                            |> List.map
                                                (\command ->
                                                    JE.object
                                                        [ ( "devices"
                                                          , command.devices
                                                                |> List.map
                                                                    (\device ->
                                                                        JE.object
                                                                            [ ( "id", JE.string device.id )
                                                                            ]
                                                                    )
                                                                |> JE.list
                                                          )
                                                        , ( "execution"
                                                          , command.execution
                                                                |> List.map
                                                                    (\execution ->
                                                                        JE.object
                                                                            [ ( "command", JE.string execution.command )
                                                                            , ( "params"
                                                                              , JE.object <|
                                                                                    List.filterMap identity
                                                                                        [ (Maybe.map (\x -> ( "on", JE.bool x )) execution.params.on)
                                                                                        , (Maybe.map (\x -> ( "color", JE.object [ ( "spectrumRGB", JE.int x ) ] )) execution.params.spectrumRGB)
                                                                                        ]
                                                                              )
                                                                            ]
                                                                    )
                                                                |> JE.list
                                                          )
                                                        ]
                                                )
                                            |> JE.list
                                      )
                                    ]
                              )
                            ]
                    )
                |> JE.list
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
                                    { on = Just onOff
                                    , spectrumRGB = Nothing
                                    }
                              }
                            ]
                      }
                    ]
                }
          }
        ]
    }


setColourRequest : DeviceId -> Float -> ExecuteRequest
setColourRequest deviceId hue =
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
                            [ { command = "action.devices.command.ColorAbsolute"
                              , params =
                                    { on = Nothing
                                    , spectrumRGB = Just <| toIntColor <| hueToColor hue
                                    }
                              }
                            ]
                      }
                    ]
                }
          }
        ]
    }


hueToColor : Float -> Color.Color
hueToColor hue =
    Color.hsl (degrees hue) 1 0.5


toIntColor : Color.Color -> Int
toIntColor c =
    let
        cc =
            Color.toRgb c
    in
        (Bitwise.shiftLeftBy 16 cc.red) + (Bitwise.shiftLeftBy 8 cc.blue) + cc.green


init : ( Model, Cmd Msg )
init =
    ( { serverUrl = "http://localhost:1234/action"
      , res = Nothing
      , devices = Dict.empty
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
            ( { model
                | res = Just r
                , devices =
                    Dict.fromList <|
                        List.map (\(Light l) -> ( l.id, Light l )) <|
                            List.map toDevice r.payload.devices
              }
            , Cmd.none
            )

        Sync _ ->
            ( model, Cmd.none )

        Exec req ->
            ( model
            , Http.send Sync <| Http.post serverUrl (Http.jsonBody <| executeRequestEncoder req) syncResponseDecoder
            )

        SetHue id hue ->
            let
                device =
                    Dict.get id model.devices
            in
                case device of
                    Just (Light l) ->
                        let
                            nl =
                                { l | hue = hue }
                        in
                            ( { model
                                | devices = Dict.insert l.id (Light nl) model.devices
                              }
                            , Http.send Sync <| Http.post serverUrl (Http.jsonBody <| executeRequestEncoder <| setColourRequest id hue) syncResponseDecoder
                            )

                    Nothing ->
                        ( model, Cmd.none )

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
                    [ Slider.view
                        [ Slider.min 0
                        , Slider.max 360
                        , Slider.value l.hue
                        , Slider.onChange <| SetHue l.id
                        ]
                    , Button.render Mdl
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
            ++ List.map (viewDevice model) (Dict.values model.devices)
        )
        |> Material.Scheme.top
