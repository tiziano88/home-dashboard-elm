module Main exposing (..)

import Bitwise
import Color
import Dict
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as JD
import Json.Encode as JE
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Scheme
import Material.Slider as Slider
import Material.Typography as Typography
import Time


type alias Model =
    { serverUrl : String
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


type alias QueryResponse =
    { requestId : String
    , payload : QueryResponsePayload
    }


type alias QueryResponsePayload =
    { devices : Dict.Dict DeviceId Params }


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
    , devices : List DeviceRequest
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
    , brightness : Maybe Float
    , thermostatMode : Maybe String
    , thermostatTemperatureSetpoint : Maybe Float
    , thermostatTemperatureAmbient : Maybe Float
    , thermostatHumidityAmbient : Maybe Float
    }


type Device
    = Light
        { id : DeviceId
        , name : String
        , hue : Float
        , brightness : Float
        , on : Bool
        }
    | Scene
        { id : DeviceId
        , name : String
        , reversible : Bool
        }
    | Thermostat
        { id : DeviceId
        , name : String
        , setTemperature : Float
        }


deviceId : Device -> DeviceId
deviceId d =
    case d of
        Light { id } ->
            id

        Scene { id } ->
            id

        Thermostat { id } ->
            id


toDevice : DeviceResponse -> Device
toDevice d =
    case d.type_ of
        "action.devices.types.LIGHT" ->
            Light
                { id = d.id
                , name = d.name.name
                , hue = 0
                , brightness = 100
                , on = False
                }

        "action.devices.types.SCENE" ->
            Scene
                { id = d.id
                , name = d.name.name
                , reversible = False
                }

        "action.devices.types.THERMOSTAT" ->
            Thermostat
                { id = d.id
                , name = d.name.name
                , setTemperature = 0
                }

        _ ->
            Scene
                { id = "xxx"
                , name = "xxx"
                , reversible = False
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


queryResponseDecoder : JD.Decoder QueryResponse
queryResponseDecoder =
    JD.map2 QueryResponse
        (JD.field "requestId" JD.string)
        (JD.field "payload" <|
            JD.map QueryResponsePayload
                (JD.field "devices" <|
                    JD.dict <|
                        JD.map7 Params
                            (JD.maybe <| JD.field "on" JD.bool)
                            (JD.maybe <| JD.field "color" (JD.field "spectrumRGB" JD.int))
                            (JD.maybe <| JD.field "brightness" JD.float)
                            (JD.maybe <| JD.field "thermostatMode" JD.string)
                            (JD.maybe <| JD.field "thermostatTemperatureSetpoint" JD.float)
                            (JD.maybe <| JD.field "thermostatTemperatureAmbient" JD.float)
                            (JD.maybe <| JD.field "thermostatHumidityAmbient" JD.float)
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
    | SetBrightness DeviceId Float
    | Query
    | QueryResponseMsg (Result Http.Error QueryResponse)


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always (Time.every Time.second (always Query))
        , view = view
        }


serverUrl =
    "https://smartlights.eu.ngrok.io/action"


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
                                    [ ( "devices"
                                      , input.payload.devices
                                            |> List.map
                                                (\device ->
                                                    JE.object
                                                        [ ( "id", JE.string device.id )
                                                        ]
                                                )
                                            |> JE.list
                                      )
                                    , ( "commands"
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
                                                                                        , (Maybe.map (\x -> ( "brightness", JE.float x )) execution.params.brightness)
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
                { devices = []
                , commands =
                    [ { devices =
                            [ { id = deviceId
                              }
                            ]
                      , execution =
                            [ { command = "action.devices.command.OnOff"
                              , params =
                                    { on = Just onOff
                                    , spectrumRGB = Nothing
                                    , brightness = Nothing
                                    , thermostatMode = Nothing
                                    , thermostatTemperatureSetpoint = Nothing
                                    , thermostatTemperatureAmbient = Nothing
                                    , thermostatHumidityAmbient = Nothing
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
                { devices = []
                , commands =
                    [ { devices =
                            [ { id = deviceId
                              }
                            ]
                      , execution =
                            [ { command = "action.devices.command.ColorAbsolute"
                              , params =
                                    { on = Nothing
                                    , spectrumRGB = Just <| toIntColor <| hueToColor hue
                                    , brightness = Nothing
                                    , thermostatMode = Nothing
                                    , thermostatTemperatureSetpoint = Nothing
                                    , thermostatTemperatureAmbient = Nothing
                                    , thermostatHumidityAmbient = Nothing
                                    }
                              }
                            ]
                      }
                    ]
                }
          }
        ]
    }


setBrightnessRequest : DeviceId -> Float -> ExecuteRequest
setBrightnessRequest deviceId brightness =
    { requestId = "11"
    , inputs =
        [ { intent = "action.devices.EXECUTE"
          , payload =
                { devices = []
                , commands =
                    [ { devices =
                            [ { id = deviceId
                              }
                            ]
                      , execution =
                            [ { command = "action.devices.command.BrightnessAbsolute"
                              , params =
                                    { on = Nothing
                                    , spectrumRGB = Nothing
                                    , brightness = Just brightness
                                    , thermostatMode = Nothing
                                    , thermostatTemperatureSetpoint = Nothing
                                    , thermostatTemperatureAmbient = Nothing
                                    , thermostatHumidityAmbient = Nothing
                                    }
                              }
                            ]
                      }
                    ]
                }
          }
        ]
    }


queryRequest : List DeviceId -> ExecuteRequest
queryRequest deviceIds =
    { requestId = "11"
    , inputs =
        [ { intent = "action.devices.QUERY"
          , payload =
                { devices = List.map (\deviceId -> { id = deviceId }) deviceIds
                , commands = []
                }
          }
        ]
    }


hueToColor : Float -> Color.Color
hueToColor hue =
    Color.hsl hue 1 0.5


hueFromColor : Color.Color -> Float
hueFromColor c =
    let
        { hue, saturation, alpha } =
            Color.toHsl c
    in
        hue


toIntColor : Color.Color -> Int
toIntColor c =
    let
        { red, green, blue, alpha } =
            Color.toRgb c
    in
        (Bitwise.shiftLeftBy 16 red) + (Bitwise.shiftLeftBy 8 green) + blue


fromIntColor : Int -> Color.Color
fromIntColor c =
    let
        red =
            Bitwise.and 0xFF <| Bitwise.shiftRightBy 16 c

        green =
            Bitwise.and 0xFF <| Bitwise.shiftRightBy 8 c

        blue =
            Bitwise.and 0xFF c
    in
        Color.rgb red green blue


toCssColor : Color.Color -> String
toCssColor c =
    let
        { red, green, blue, alpha } =
            Color.toRgb c
    in
        "rgb(" ++ (toString red) ++ "," ++ (toString green) ++ "," ++ (toString blue) ++ ")"


init : ( Model, Cmd Msg )
init =
    ( { serverUrl = serverUrl
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

        QueryResponseMsg (Ok r) ->
            -- Point-Free FTW! :D
            let
                updateDevice params =
                    let
                        updateBrightness device =
                            case params.brightness of
                                Just b ->
                                    case device of
                                        Light l ->
                                            Light { l | brightness = b }

                                        _ ->
                                            device

                                Nothing ->
                                    device

                        updateHue device =
                            case params.spectrumRGB of
                                Just b ->
                                    case device of
                                        Light l ->
                                            Light { l | hue = hueFromColor <| fromIntColor <| b }

                                        _ ->
                                            device

                                Nothing ->
                                    device

                        updateOn device =
                            case params.on of
                                Just b ->
                                    case device of
                                        Light l ->
                                            Light { l | on = b }

                                        _ ->
                                            device

                                Nothing ->
                                    device

                        updateTemperatureSetpoint device =
                            case params.thermostatTemperatureSetpoint of
                                Just b ->
                                    case device of
                                        Thermostat t ->
                                            Thermostat { t | setTemperature = b }

                                        _ ->
                                            device

                                Nothing ->
                                    device
                    in
                        updateHue << updateBrightness << updateOn << updateTemperatureSetpoint

                devices =
                    r.payload.devices
                        |> Dict.toList
                        |> List.foldl
                            (\( id, params ) -> Dict.update id (Maybe.map (updateDevice params)))
                            model.devices
            in
                ( { model | devices = devices }, Cmd.none )

        QueryResponseMsg _ ->
            ( model, Cmd.none )

        Sync (Ok r) ->
            let
                devices =
                    Dict.fromList <|
                        List.map (\d -> ( deviceId d, d )) <|
                            List.map toDevice r.payload.devices
            in
                ( { model | devices = devices }
                , Http.send QueryResponseMsg <|
                    Http.post
                        serverUrl
                        (Http.jsonBody <| executeRequestEncoder <| queryRequest <| Dict.keys devices)
                        queryResponseDecoder
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

                    _ ->
                        ( model, Cmd.none )

        SetBrightness id brightness ->
            let
                device =
                    Dict.get id model.devices
            in
                case device of
                    Just (Light l) ->
                        let
                            nl =
                                { l | brightness = brightness }
                        in
                            ( { model
                                | devices = Dict.insert l.id (Light nl) model.devices
                              }
                            , Http.send Sync <|
                                Http.post
                                    serverUrl
                                    (Http.jsonBody <| executeRequestEncoder <| setBrightnessRequest id brightness)
                                    syncResponseDecoder
                            )

                    _ ->
                        ( model, Cmd.none )

        Query ->
            ( model
            , Http.send QueryResponseMsg <|
                Http.post
                    serverUrl
                    (Http.jsonBody <| executeRequestEncoder <| queryRequest <| Dict.keys model.devices)
                    queryResponseDecoder
            )

        Nop ->
            ( model, Cmd.none )


viewDevice : Model -> Device -> Html.Html Msg
viewDevice model d =
    case d of
        Light l ->
            Card.view
                [ css "width" "20em"
                , css "margin" "1em"
                , Elevation.e8
                ]
                [ Card.title []
                    [ Card.head [] [ Html.text l.name ]
                    ]
                , Card.actions
                    [ css "background-color" <| toCssColor <| hueToColor l.hue
                    ]
                    []
                , Card.actions []
                    [ Html.text "Hue"
                    , Slider.view
                        [ Slider.min 0
                        , Slider.max (2 * pi)
                        , Slider.step 0.1
                        , Slider.value l.hue
                        , Slider.onChange <| SetHue l.id
                        ]
                    , Html.text "Brightness"
                    , Slider.view
                        [ Slider.min 0
                        , Slider.max 100
                        , Slider.step 10
                        , Slider.value l.brightness
                        , Slider.onChange <| SetBrightness l.id
                        ]
                    , Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Options.onClick <| Exec <| onOffRequest l.id False
                        ]
                        [ Html.text "off" ]
                    , Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Options.onClick <| Exec <| onOffRequest l.id True
                        ]
                        [ Html.text "on" ]
                    ]
                ]

        Scene s ->
            Card.view
                [ css "width" "20em"
                , css "margin" "1em"
                , Elevation.e8
                ]
                [ Card.title []
                    [ Card.head [] [ Html.text s.name ]
                    ]
                , Card.actions []
                    [ Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Options.onClick <| Exec <| onOffRequest s.id False
                        ]
                        [ Html.text "off" ]
                    , Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Options.onClick <| Exec <| onOffRequest s.id True
                        ]
                        [ Html.text "on" ]
                    ]
                ]

        Thermostat t ->
            Card.view
                [ css "width" "20em"
                , css "margin" "1em"
                , Elevation.e8
                ]
                [ Card.title []
                    [ Card.head [] [ Html.text t.name ]
                    , Options.div
                        []
                        [ Options.span
                            [ Typography.display4
                            , Material.Color.text Material.Color.primary
                            ]
                            [ Html.text <| (toString t.setTemperature) ++ "°C" ]
                        ]
                    ]
                , Card.actions []
                    [ Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Options.onClick <| Exec <| onOffRequest t.id False
                        ]
                        [ Html.text "off" ]
                    , Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Options.onClick <| Exec <| onOffRequest t.id True
                        ]
                        [ Html.text "on" ]
                    ]
                ]


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style
            [ ( "background-color", "#FAFAFA" ) ]
        ]
        ([ --, Button.render a
           Html.node "meta"
            [ Html.Attributes.name "viewport"
            , Html.Attributes.content "width=430px, initial-scale=1"
            ]
            []
         ]
            ++ List.map (viewDevice model) (Dict.values model.devices)
        )
        |> Material.Scheme.top
