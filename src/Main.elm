port module Main exposing (HoverLinesPosition, Model, Msg(..), SvgMouseCoordinate, decodeSvgMouseCoordinate, init, main, onSvgMouseMove, subscriptions, transparentColor, update, view, viewHoverLines, viewPath, viewSvg)

import Axis
import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Path exposing (Path)
import Scale
import Shape
import SubPath exposing (SubPath)
import Svg.Attributes exposing (id)
import Svg.Events
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (..)


main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.none


init : Decode.Value -> ( Model, Cmd msg )
init flags =
    ( { hoverLinesPosition = Nothing
      , curveValues = generateBeanTemperatureCurve 900000
      }
    , Cmd.none
    )


type Msg
    = UpdateHoverLinesPosition SvgMouseCoordinate
    | HideHoverLines
    | AddAdjustPoint


type alias HoverLinesPosition =
    { horizontal : Float
    , vertical : Float
    }


type alias Model =
    { hoverLinesPosition : Maybe HoverLinesPosition
    , curveValues : List ( Float, Float )
    }


update : Msg -> Model -> ( Model, Cmd cmd )
update msg model =
    case msg of
        UpdateHoverLinesPosition { x, y } ->
            let
                hoverLinesPosition =
                    { horizontal = x
                    , vertical = y
                    }
            in
            ( { model | hoverLinesPosition = Just hoverLinesPosition }, Cmd.none )

        HideHoverLines ->
            ( { model | hoverLinesPosition = Nothing }, Cmd.none )

        AddAdjustPoint ->
            let
                newCurveValue =
                    case model.hoverLinesPosition of
                        Just { horizontal, vertical } ->
                            ( horizontal * 1000, vertical )

                        Nothing ->
                            ( 400000, 420 )

                newCurveValues =
                    [ newCurveValue ]
                        |> List.append model.curveValues
                        |> List.sortBy Tuple.first
            in
            ( { model | curveValues = newCurveValues }, Cmd.none )


outerWidth : Float
outerWidth =
    960.0


aspectRatio : Float
aspectRatio =
    9.0 / 16.0


outerHeight : Float
outerHeight =
    outerWidth * aspectRatio


margin : Float
margin =
    50.0


innerHeight : Float
innerHeight =
    outerHeight - margin * 2.0


innerWidth : Float
innerWidth =
    outerWidth - margin * 2.0


view : Model -> Html Msg
view model =
    div []
        [ viewSvg model
        , text <| Debug.toString model
        ]


transparentColor : Color.Color
transparentColor =
    Color.fromRgba { red = 0.0, blue = 0.0, green = 0.0, alpha = 0.0 }


viewSvg : Model -> Html Msg
viewSvg model =
    let
        minValue : Float
        minValue =
            model.curveValues
                |> List.map Tuple.second
                |> List.minimum
                |> Maybe.withDefault 0.0

        maxValue : Float
        maxValue =
            model.curveValues
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0.0

        minTime : Float
        minTime =
            model.curveValues
                |> List.map Tuple.first
                |> List.minimum
                |> Maybe.withDefault 0.0

        maxTime : Float
        maxTime =
            model.curveValues
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0.0

        timeScale : Scale.ContinuousScale Float
        timeScale =
            Scale.linear ( 0.0, innerWidth ) ( minTime, maxTime )

        valueScale : Scale.ContinuousScale Float
        valueScale =
            Scale.linear ( innerHeight, 0.0 ) ( minValue, maxValue )

        adjustPoints : List ( Float, Float )
        adjustPoints =
            model.curveValues
                |> List.map
                    (\( time, value ) ->
                        ( Scale.convert timeScale time
                        , Scale.convert valueScale value
                        )
                    )
    in
    svg
        [ class [ "ce-pe-all" ]
        , TypedSvg.Attributes.width <| percent 100
        , preserveAspectRatio (Align ScaleMid ScaleMin) Slice
        , viewBox 0 0 outerWidth outerHeight
        ]
        [ g
            [ transform [ Translate margin margin ]
            ]
            [ -- lines area group
              g
                [ class [ "ce-pe-all" ]
                , onMouseLeave HideHoverLines
                ]
                [ -- The above <g> element doesn't catch mouse events.
                  -- That's why we need this rectangle to fill the inner
                  -- space to trigger mouse events
                  rect
                    [ class [ "ce-pe-all" ]
                    , width innerWidth
                    , height innerHeight
                    , fill <| Fill transparentColor
                    , onSvgMouseMove UpdateHoverLinesPosition
                    , TypedSvg.Events.onClick AddAdjustPoint
                    ]
                    []
                , viewHoverLines model.hoverLinesPosition
                , viewPath adjustPoints timeScale valueScale
                , viewAdjustPoints adjustPoints
                , g
                    [ transform [ Translate 0 innerHeight ]
                    ]
                    [ Axis.bottom [ Axis.tickCount 10 ] timeScale
                    ]
                , g
                    [ transform [ Translate 0 0 ]
                    ]
                    [ Axis.left [ Axis.tickCount 10 ] valueScale
                    ]
                ]
            ]
        ]


viewPath curveValues timeScale valueScale =
    let
        linePath : SubPath
        linePath =
            Shape.catmullRomCurve 0.4 curveValues
    in
    SubPath.element linePath
        [ class [ "ce-pe-none" ]
        , style "pointer-events: none"
        , stroke Color.blue
        , strokeWidth <| px 2
        , fill FillNone
        ]


viewAdjustPoints curveValues =
    g []
        (curveValues
            |> List.map
                (\( coordinateX, coordinateY ) ->
                    circle
                        [ class [ "ce-pe-none" ]
                        , cx <| px coordinateX
                        , cy <| px coordinateY
                        , r <| px 5
                        , style "pointer-events: none"
                        , stroke Color.blue
                        , strokeWidth <| px 2
                        , fill FillNone
                        ]
                        []
                )
        )


viewHoverLines : Maybe HoverLinesPosition -> Html msg
viewHoverLines hoverLinesPosition =
    case hoverLinesPosition of
        Nothing ->
            text ""

        Just position ->
            g []
                [ -- horizontal line
                  line
                    [ x1 <| px 0
                    , x2 <| px innerWidth
                    , y1 <| px position.vertical
                    , y2 <| px position.vertical
                    , style "pointer-events: none"
                    , class [ "ce-pe-none" ]
                    , strokeWidth <| px 2
                    , stroke Color.black
                    , opacity <| Opacity 0.1
                    ]
                    []

                -- vertical line
                , line
                    [ x1 <| px position.horizontal
                    , x2 <| px position.horizontal
                    , y1 <| px 0
                    , y2 <| px innerHeight
                    , style "pointer-events: none"
                    , class [ "ce-pe-none" ]
                    , strokeWidth <| px 2
                    , stroke Color.black
                    , opacity <| Opacity 0.1
                    ]
                    []
                ]



-- HELPER FUNCTIONS


onSvgMouseMove message =
    Svg.Events.on "mousemoveWithCoordinates" (Decode.map message decodeSvgMouseCoordinate)


type alias SvgMouseCoordinate =
    { x : Float
    , y : Float
    }


decodeSvgMouseCoordinate : Decode.Decoder SvgMouseCoordinate
decodeSvgMouseCoordinate =
    Decode.map2 SvgMouseCoordinate
        (Decode.at [ "detail", "x" ] Decode.float)
        (Decode.at [ "detail", "y" ] Decode.float)



-- HELPER DATA


generateBeanTemperatureCurve : Int -> List ( Float, Float )
generateBeanTemperatureCurve duration =
    [ ( 0.0, 440.0 )
    , ( toFloat duration * 0.15, 380.0 )
    , ( toFloat duration * 0.65, 442.0 )
    , ( toFloat duration, 455.0 )
    ]
