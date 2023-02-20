module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, input, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, field, int, map, map4, string)
import Maybe exposing (Maybe)
import String exposing (String)


main =
    Browser.sandbox { init = init, update = update, view = view }


type DefinitionValidity
    = ValidDefinition TrackerTopLevelSchema
    | InvalidDefinition Decode.Error
    | StartingOut


type Value
    = WholeNumber Int


valueToString : Value -> String
valueToString v =
    case v of
        WholeNumber n ->
            String.fromInt n


type alias TrackingState =
    Dict String Value


type Error
    = ComponentIdNotFound String


type Stage
    = DefinitionStage String DefinitionValidity
    | TrackerStage TrackerTopLevelSchema TrackingState (Maybe Error)


type alias Model =
    Stage


init : Model
init =
    DefinitionStage "" StartingOut


type TrackMsg
    = ApplyEffects (List Effect)


type Msg
    = UpdateDefinition String
    | CreateTracker TrackerTopLevelSchema
    | TestTracker String
    | TrackerMsg TrackerTopLevelSchema TrackingState TrackMsg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateDefinition def ->
            case Decode.decodeString trackerTopLevelSchemaDecoder def of
                Err e ->
                    DefinitionStage def <| InvalidDefinition e

                Ok d ->
                    DefinitionStage def <| ValidDefinition d

        TestTracker def ->
            case Decode.decodeString trackerTopLevelSchemaDecoder def of
                Err e ->
                    DefinitionStage def <| InvalidDefinition e

                Ok d ->
                    TrackerStage d Dict.empty Nothing

        CreateTracker def ->
            TrackerStage def Dict.empty Nothing

        TrackerMsg schema state (ApplyEffects effects) ->
            case List.foldl applyEffect (Ok ( schema, state )) effects of
                Ok ( sc, st ) ->
                    TrackerStage sc st Nothing

                Err e ->
                    TrackerStage schema state (Just e)


applyEffect : Effect -> Result Error ( TrackerTopLevelSchema, TrackingState ) -> Result Error ( TrackerTopLevelSchema, TrackingState )
applyEffect eff result =
    case result of
        Err e ->
            Err e

        Ok ( schema, state ) ->
            case eff of
                RestoreDefaults { items } ->
                    Result.map (\v -> ( schema, v )) (List.foldl (restoreDefault schema) (Ok state) items)


restoreDefault : TrackerTopLevelSchema -> EffectTarget -> Result Error TrackingState -> Result Error TrackingState
restoreDefault schema target result =
    case result of
        Err e ->
            Err e

        Ok state ->
            case target of
                ById targetId ->
                    case idDefault targetId schema.tracker of
                        Just default ->
                            Ok <| Dict.insert targetId default state

                        Nothing ->
                            Err <| ComponentIdNotFound targetId


view : Model -> Html Msg
view model =
    case model of
        DefinitionStage def valid ->
            viewEditTracker def valid

        TrackerStage def state err ->
            viewTracker def state err


viewEditTracker : String -> DefinitionValidity -> Html Msg
viewEditTracker def valid =
    div []
        [ div [] [ h1 [] [ text "New Tracker" ] ]
        , div [] [ textarea [ cols 40, rows 10, placeholder "...", onInput UpdateDefinition ] [] ]
        , case valid of
            ValidDefinition d ->
                div []
                    [ div [] [ text <| "Definition good: " ++ d.name ]
                    , button [ onClick (CreateTracker d) ] [ text "Create!" ]
                    ]

            InvalidDefinition err ->
                text ("Definition invalid: " ++ Decode.errorToString err)

            StartingOut ->
                text ""
        , div [] [ h1 [] [ text "Test Tracker" ] ]
        , div [] [ button [ onClick (TestTracker simpleDominionTracker) ] [ text "Dominion turn tracker" ] ]
        ]


viewTrackerComponent : TrackerSchema -> TrackingState -> Html TrackMsg
viewTrackerComponent tracker state =
    case tracker of
        WholeNumberSchema s ->
            div [] [ text s.text, input [ type_ "number", value (valueToString <| Maybe.withDefault s.default (Dict.get s.id state)) ] [] ]

        Group s ->
            div [] (List.map (\i -> viewTrackerComponent i state) s.items)

        Action s ->
            button [ onClick (ApplyEffects s.effects) ] [ text s.text ]


viewTracker : TrackerTopLevelSchema -> TrackingState -> Maybe Error -> Html Msg
viewTracker schema state err =
    case err of
        Just (ComponentIdNotFound id) ->
            text <| "Component ID could not be found: " ++ id

        Nothing ->
            div []
                [ h1 [] [ text schema.name ]
                , Html.map (TrackerMsg schema state) <| viewTrackerComponent schema.tracker state
                ]



-- Tracker Schema


effectTargetDecoder : Decoder EffectTarget
effectTargetDecoder =
    Decode.map ById <| field "id" string


type EffectTarget
    = ById String


effectDecoder : Decoder Effect
effectDecoder =
    Decode.map (\items -> RestoreDefaults { items = items }) <| field "items" (Decode.list effectTargetDecoder)


type Effect
    = RestoreDefaults { items : List EffectTarget }


groupDecoder : Decoder TrackerSchema
groupDecoder =
    Decode.map (\items -> Group { items = items }) <| field "items" (Decode.list trackerSchemaDecoder)


actionDecoder : Decoder TrackerSchema
actionDecoder =
    Decode.map2
        (\text effects -> Action { text = text, effects = effects })
        (field "text" string)
        (field "effects" (Decode.list effectDecoder))


wholeNumberDecoder : Decoder TrackerSchema
wholeNumberDecoder =
    Decode.map3
        (\text default id -> WholeNumberSchema { text = text, default = default, id = id })
        (field "text" string)
        (field "default" (Decode.map WholeNumber Decode.int))
        (field "id" string)


trackerSchemaDecoder : Decoder TrackerSchema
trackerSchemaDecoder =
    Decode.lazy <| \_ -> Decode.oneOf [ groupDecoder, actionDecoder, wholeNumberDecoder ]


type TrackerSchema
    = Group { items : List TrackerSchema }
    | Action { text : String, effects : List Effect }
    | WholeNumberSchema { text : String, default : Value, id : String }


idDefault : String -> TrackerSchema -> Maybe Value
idDefault id schema =
    case schema of
        WholeNumberSchema s ->
            if s.id == id then
                Just s.default

            else
                Nothing

        Group s ->
            List.head <| List.filterMap (idDefault id) s.items

        Action s ->
            Nothing


trackerTopLevelSchemaDecoder : Decoder TrackerTopLevelSchema
trackerTopLevelSchemaDecoder =
    Decode.map2 TrackerTopLevelSchema (field "name" string) (field "tracker" trackerSchemaDecoder)


type alias TrackerTopLevelSchema =
    { name : String
    , tracker : TrackerSchema
    }


simpleDominionTracker : String
simpleDominionTracker =
    """
{
  "name": "Dominion Turn Tracker",
  "tracker": {
    "type": "group",
    "items": [
      {
        "type": "action",
        "text": "Reset",
        "effects": [
          {
            "type": "restore-defaults",
            "items": [
              {
                "id": "a"
              },
              {
                "id": "b"
              },
              {
                "id": "em"
              }
            ]
          }
        ]
      },
      {
        "type": "number",
        "text": "Actions",
        "default": 1,
        "id": "a"
      },
      {
        "type": "number",
        "text": "Buys",
        "default": 1,
        "id": "b"
      },
      {
        "type": "number",
        "text": "Extra Money",
        "default": 0,
        "id": "em"
      }
    ]
  }
}
              """
