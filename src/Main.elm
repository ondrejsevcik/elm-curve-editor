module Main exposing (..)

import Axis
import Browser
import Color
import Html
import Html.Attributes
import Html.Events as HE
import Json.Decode as Decode
import RoastCurve
import Scale
import Shape
import SubPath
import Svg
import Svg.Events
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx
import TypedSvg.Core
import TypedSvg.Events
import TypedSvg.Types as SvgTypes


main : Program Decode.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type ActiveValue
    = Selected RoastCurve.Value
    | Dragging RoastCurve.Value
    | NoValue


init : Decode.Value -> ( Model, Cmd msg )
init flags =
    ( { mousePosition = Nothing
      , curveValues = RoastCurve.newFromDuration 900000
      , activeValue = NoValue
      }
    , Cmd.none
    )


type Msg
    = UpdateMousePosition (Maybe MousePosition)
    | AddAdjustPoint
    | SelectAdjustPoint RoastCurve.Value
    | DeselectSelectedAdjustPoint
    | DragStartAdjustPoint RoastCurve.Value
    | StopDraggingAdjustPoint RoastCurve.Value
    | DeleteSelectedValue


type alias MousePosition =
    { cx : Float
    , cy : Float
    }


type alias Model =
    { mousePosition : Maybe MousePosition
    , curveValues : RoastCurve.CurveValues
    , activeValue : ActiveValue
    }


update : Msg -> Model -> ( Model, Cmd cmd )
update msg model =
    case msg of
        UpdateMousePosition newMousePosition ->
            let
                updatedCurveValues =
                    case ( model.mousePosition, model.activeValue ) of
                        ( Just { cx, cy }, Dragging value ) ->
                            let
                                ( timeScale, valueScale ) =
                                    getScales model.curveValues

                                updatedAdjustPoint =
                                    ( Scale.invert timeScale cx
                                    , Scale.invert valueScale cy
                                    )
                            in
                            RoastCurve.updateValue model.curveValues value updatedAdjustPoint

                        _ ->
                            model.curveValues
            in
            ( { model
                | mousePosition = newMousePosition
                , curveValues = updatedCurveValues
              }
            , Cmd.none
            )

        AddAdjustPoint ->
            case model.mousePosition of
                Just { cx, cy } ->
                    let
                        ( timeScale, valueScale ) =
                            getScales model.curveValues

                        newCurveValue =
                            ( Scale.invert timeScale cx
                            , Scale.invert valueScale cy
                            )

                        ( updatedCurveValues, newValue ) =
                            RoastCurve.insertValue model.curveValues newCurveValue
                    in
                    ( { model
                        | curveValues = updatedCurveValues
                        , activeValue = Selected newValue
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        SelectAdjustPoint value ->
            ( { model | activeValue = Dragging value }, Cmd.none )

        DeselectSelectedAdjustPoint ->
            ( { model | activeValue = NoValue }, Cmd.none )

        DragStartAdjustPoint value ->
            ( { model | activeValue = Dragging value }, Cmd.none )

        StopDraggingAdjustPoint value ->
            ( { model | activeValue = Selected value }, Cmd.none )

        DeleteSelectedValue ->
            case model.activeValue of
                Selected value ->
                    let
                        updatedCurveValues =
                            RoastCurve.removeValue model.curveValues value
                    in
                    ( { model
                        | curveValues = updatedCurveValues
                        , activeValue = NoValue
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


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


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewSvg model
        , Html.div
            []
            [ Html.button
                [ HE.onClick DeleteSelectedValue
                , Html.Attributes.disabled (model.activeValue == NoValue)
                ]
                [ Html.text "Delete selected" ]
            , Html.span
                [ SvgAttr.style "margin-left: 1rem" ]
                [ Html.text "Tip: double click to add new point" ]
            ]
        , Html.div
            [ SvgAttr.style "max-width:48rem; background:beige; padding:1rem;"
            ]
            [ Html.text <| Debug.toString model
            ]
        ]


transparentColor : Color.Color
transparentColor =
    Color.fromRgba { red = 0.0, blue = 0.0, green = 0.0, alpha = 0.0 }


viewSvg : Model -> Html.Html Msg
viewSvg model =
    let
        ( timeScale, temperatureScale ) =
            getScales model.curveValues

        svgPoints : List ( RoastCurve.Value, RoastCurve.RawValue )
        svgPoints =
            RoastCurve.toList model.curveValues
                |> List.map
                    (\v ->
                        ( v
                        , ( Scale.convert timeScale (RoastCurve.timeFor v)
                          , Scale.convert temperatureScale (RoastCurve.temperatureFor v)
                          )
                        )
                    )
    in
    svg
        [ SvgAttr.width <| SvgTypes.percent 100
        , SvgAttr.style ("max-width:900px")
        , SvgAttr.preserveAspectRatio (SvgTypes.Align SvgTypes.ScaleMid SvgTypes.ScaleMin) SvgTypes.Slice
        , SvgAttr.viewBox 0 0 outerWidth outerHeight
        ]
        [ g
            [ SvgAttr.transform [ SvgTypes.Translate margin margin ]
            ]
            [ -- lines area group
              g
                [ SvgAttr.style "pointer-events: all;"
                ]
                [ -- The above <g> element doesn't catch mouse events.
                  -- That's why we need this rectangle to fill the inner
                  -- space to trigger mouse events
                  rect
                    [ SvgAttr.style "pointer-events: all;"
                    , TypedSvg.Attributes.InPx.width innerWidth
                    , TypedSvg.Attributes.InPx.height innerHeight
                    , SvgAttr.fill <| SvgTypes.Fill transparentColor
                    , onSvgMouseMove UpdateMousePosition
                    , TypedSvg.Events.onMouseLeave (UpdateMousePosition Nothing)
                    , onDoubleClick AddAdjustPoint
                    , TypedSvg.Events.onClick DeselectSelectedAdjustPoint
                    ]
                    []
                , g
                    [ SvgAttr.transform [ SvgTypes.Translate 0 innerHeight ]
                    ]
                    [ Axis.bottom [ Axis.tickCount 10 ] timeScale
                    ]
                , g
                    [ SvgAttr.transform [ SvgTypes.Translate 0 0 ]
                    ]
                    [ Axis.left [ Axis.tickCount 10 ] temperatureScale
                    ]
                , viewHoverLines model.mousePosition
                , viewPath svgPoints
                , viewAdjustPoints model.activeValue svgPoints
                ]
            ]
        ]


viewPath : SvgPoints -> TypedSvg.Core.Svg Msg
viewPath svgPoints =
    let
        linePath : SubPath.SubPath
        linePath =
            Shape.catmullRomCurve 0.4 (List.map Tuple.second svgPoints)
    in
    SubPath.element linePath
        [ SvgAttr.style "pointer-events: none"
        , SvgAttr.stroke Color.blue
        , SvgAttr.strokeWidth <| SvgTypes.px 2
        , SvgAttr.fill SvgTypes.FillNone
        ]


type alias SvgPoints =
    List ( RoastCurve.Value, RoastCurve.RawValue )


viewAdjustPoints : ActiveValue -> SvgPoints -> TypedSvg.Core.Svg Msg
viewAdjustPoints activeValue svgPoints =
    let
        circles =
            svgPoints
                |> List.map
                    (\( value, ( coordinateX, coordinateY ) ) ->
                        let
                            isSelected =
                                activeValue == Selected value || activeValue == Dragging value
                        in
                        circle
                            [ SvgAttr.cx <| SvgTypes.px coordinateX
                            , SvgAttr.cy <| SvgTypes.px coordinateY
                            , SvgAttr.r <| SvgTypes.px 5
                            , SvgAttr.style "pointer-events: all"
                            , SvgAttr.stroke Color.blue
                            , SvgAttr.strokeWidth <| SvgTypes.px 2
                            , SvgAttr.fill
                                (if isSelected then
                                    SvgTypes.Fill Color.green

                                 else
                                    SvgTypes.Fill Color.white
                                )
                            , TypedSvg.Events.onMouseDown (SelectAdjustPoint value)
                            , TypedSvg.Events.onMouseUp (StopDraggingAdjustPoint value)
                            , onSvgMouseMove UpdateMousePosition
                            ]
                            []
                    )
    in
    g [] circles


viewHoverLines : Maybe MousePosition -> Html.Html msg
viewHoverLines mousePosition =
    case mousePosition of
        Nothing ->
            Html.text ""

        Just position ->
            g []
                [ -- horizontal line
                  line
                    [ SvgAttr.x1 <| SvgTypes.px 0
                    , SvgAttr.x2 <| SvgTypes.px innerWidth
                    , SvgAttr.y1 <| SvgTypes.px position.cy
                    , SvgAttr.y2 <| SvgTypes.px position.cy
                    , SvgAttr.style "pointer-events: none"
                    , SvgAttr.strokeWidth <| SvgTypes.px 2
                    , SvgAttr.stroke Color.black
                    , SvgAttr.opacity <| SvgTypes.Opacity 0.1
                    ]
                    []

                -- vertical line
                , line
                    [ SvgAttr.x1 <| SvgTypes.px position.cx
                    , SvgAttr.x2 <| SvgTypes.px position.cx
                    , SvgAttr.y1 <| SvgTypes.px 0
                    , SvgAttr.y2 <| SvgTypes.px innerHeight
                    , SvgAttr.style "pointer-events: none"
                    , SvgAttr.strokeWidth <| SvgTypes.px 2
                    , SvgAttr.stroke Color.black
                    , SvgAttr.opacity <| SvgTypes.Opacity 0.1
                    ]
                    []
                ]



-- GRAPH HELPER FUNCTIONS


getScales : RoastCurve.CurveValues -> ( Scale.ContinuousScale Float, Scale.ContinuousScale Float )
getScales curveValues =
    let
        xScale : Scale.ContinuousScale Float
        xScale =
            Scale.linear
                ( 0.0, innerWidth )
                ( RoastCurve.minTime curveValues, RoastCurve.maxTime curveValues )

        yScale : Scale.ContinuousScale Float
        yScale =
            Scale.linear
                ( innerHeight, 0.0 )
                ( RoastCurve.minValue curveValues, RoastCurve.maxValue curveValues )
    in
    ( xScale, yScale )



-- HELPER FUNCTIONS


{-| Handler for native browser 'dblclick' event
-}
onDoubleClick : msg -> Svg.Attribute msg
onDoubleClick message =
    Svg.Events.on "dblclick" (Decode.succeed message)


{-| Custom event defined in the index.js
-}
onSvgMouseMove : (Maybe MousePosition -> msg) -> Svg.Attribute msg
onSvgMouseMove message =
    Svg.Events.on "mouseMoveWithCoordinates" (Decode.map message decodeMousePosition)


decodeMousePosition : Decode.Decoder (Maybe MousePosition)
decodeMousePosition =
    Decode.maybe <|
        Decode.map2 MousePosition
            (Decode.at [ "detail", "x" ] Decode.float)
            (Decode.at [ "detail", "y" ] Decode.float)
