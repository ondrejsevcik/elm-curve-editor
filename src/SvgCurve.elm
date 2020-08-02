module SvgCurve exposing
    ( CurveValues(..)
    , RawValue
    , Value(..)
    , ValueId
    , getNewId
    , insertValue
    , maxTime
    , maxValue
    , minTime
    , minValue
    , newFromDuration
    , newFromList
    , removeValue
    , temperatureFor
    , timeFor
    , toList
    , updateValue
    )

import Dict exposing (Dict)


type CurveValues
    = CurveValues (Dict.Dict ValueId Value)


type alias ValueId =
    Int


type Value
    = Value
        { id : ValueId
        , time : Float
        , temperature : Float
        }


newFromList : List ( Float, Float ) -> CurveValues
newFromList values =
    let
        initialValues =
            List.map
                (\( time, temperature ) ->
                    { time = time, temperature = temperature }
                )
                values
    in
    initialValues
        |> List.indexedMap
            (\index value ->
                ( index
                , Value
                    { id = index
                    , time = value.time
                    , temperature = value.temperature
                    }
                )
            )
        |> Dict.fromList
        |> CurveValues


newFromDuration : Int -> CurveValues
newFromDuration duration =
    let
        initialValues =
            [ { time = 0.0, temperature = 440.0 }
            , { time = toFloat duration * 0.15, temperature = 380.0 }
            , { time = toFloat duration * 0.65, temperature = 442.0 }
            , { time = toFloat duration, temperature = 455.0 }
            ]
    in
    initialValues
        |> List.indexedMap
            (\index value ->
                ( index
                , Value
                    { id = index
                    , time = value.time
                    , temperature = value.temperature
                    }
                )
            )
        |> Dict.fromList
        |> CurveValues


minTime : CurveValues -> Float
minTime (CurveValues curveValues) =
    curveValues
        |> Dict.values
        |> List.map (\(Value v) -> v.time)
        |> List.minimum
        |> Maybe.withDefault 0.0


maxTime : CurveValues -> Float
maxTime (CurveValues curveValues) =
    curveValues
        |> Dict.values
        |> List.map (\(Value v) -> v.time)
        |> List.maximum
        |> Maybe.withDefault 0.0


minValue : CurveValues -> Float
minValue (CurveValues curveValues) =
    curveValues
        |> Dict.values
        |> List.map (\(Value v) -> v.temperature)
        |> List.minimum
        |> Maybe.withDefault 0.0


maxValue : CurveValues -> Float
maxValue (CurveValues curveValues) =
    curveValues
        |> Dict.values
        |> List.map (\(Value v) -> v.temperature)
        |> List.maximum
        |> Maybe.withDefault 0.0


toList : CurveValues -> List Value
toList (CurveValues curveValues) =
    curveValues
        |> Dict.values
        |> List.sortBy (\(Value v) -> v.time)


timeFor : Value -> Float
timeFor (Value v) =
    v.time


temperatureFor : Value -> Float
temperatureFor (Value v) =
    v.temperature


type alias RawValue =
    ( Float, Float )


updateValue : CurveValues -> Value -> RawValue -> CurveValues
updateValue (CurveValues curveValues) ((Value { id }) as currentValue) ( time, temperature ) =
    Dict.insert
        id
        (Value { id = id, time = time, temperature = temperature })
        curveValues
        |> CurveValues


insertValue : CurveValues -> RawValue -> ( CurveValues, Value )
insertValue ((CurveValues curveValues) as cv) ( time, temperature ) =
    let
        id : ValueId
        id =
            Debug.log "newId" (getNewId cv)

        newValue =
            Value
                { id = id
                , time = time
                , temperature = temperature
                }

        updatedCurveValues =
            CurveValues (Dict.insert id newValue curveValues)
    in
    ( updatedCurveValues, newValue )


removeValue : CurveValues -> Value -> CurveValues
removeValue (CurveValues curveValues) (Value { id }) =
    CurveValues (Dict.remove id curveValues)


{-| TODO this is totally wrong, add tests |
-}
getNewId : CurveValues -> ValueId
getNewId (CurveValues curveValues) =
    let
        lastId : Int
        lastId =
            Debug.log "size" (Dict.size curveValues - 1)

        lastValue : Maybe Value
        lastValue =
            Dict.get lastId curveValues
    in
    case lastValue of
        Just (Value value) ->
            value.id + 1

        Nothing ->
            1
