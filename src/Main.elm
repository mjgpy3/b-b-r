module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as Decode exposing (Decoder, field, int, map, map4, string)


main =
    Browser.sandbox { init = init, update = update, view = view }


type DefinitionValidity
    = ValidDefinition TrackerTopLevelSchema
    | InvalidDefinition (Maybe Decode.Error)


type Stage
    = DefinitionStage String DefinitionValidity
    | TrackerStage


type alias Model =
    Stage


init : Model
init =
    DefinitionStage "" <| InvalidDefinition Nothing


type Msg
    = UpdateDefinition String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateDefinition def ->
            case Decode.decodeString trackerTopLevelSchemaDecoder def of
              Err e -> DefinitionStage def <| InvalidDefinition (Just e)
              Ok d -> DefinitionStage def <| ValidDefinition d


view : Model -> Html Msg
view model =
    case model of
        DefinitionStage def valid ->
            div []
                [ div [] [ h1 [] [ text "New Tracker" ] ]
                , div [] [ textarea [ cols 40, rows 10, placeholder "...", onInput UpdateDefinition ] [] ]
                , case ( def, valid ) of
                    ( "", _ ) ->
                        text ""

                    ( _, ValidDefinition d) ->
                        text <| "Definition good: " ++ d.name

                    ( _, InvalidDefinition (Just err)) ->
                        text ("Definition invalid: " ++ Decode.errorToString err)
                    ( _, InvalidDefinition Nothing) ->
                        text ("Definition invalid")
                ]

        TrackerStage ->
            div []
                [ text "tracking" ]



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
    Decode.map (\items -> Group {items = items}) <| field "items" (Decode.list trackerSchemaDecoder)

resetActionDecoder : Decoder TrackerSchema
resetActionDecoder =
    Decode.map2
        (\text effects -> ResetAction {text=text, effects=effects})
        (field "text" string)
        (field "effects" (Decode.list effectDecoder))

wholeNumberDecoder : Decoder TrackerSchema
wholeNumberDecoder =
    Decode.map3
        (\text default id -> WholeNumber {text=text, default=default, id=id})
        (field "text" string)
        (field "default" Decode.int)
        (field "id" (Decode.maybe string))

trackerSchemaDecoder : Decoder TrackerSchema
trackerSchemaDecoder =
    Decode.lazy <| \_ -> Decode.oneOf [groupDecoder, resetActionDecoder, wholeNumberDecoder]

type TrackerSchema
    = Group { items : List TrackerSchema }
    | ResetAction { text : String, effects : List Effect }
    | WholeNumber { text : String, default : Int, id : Maybe String }


trackerTopLevelSchemaDecoder : Decoder TrackerTopLevelSchema
trackerTopLevelSchemaDecoder =
    Decode.map2 TrackerTopLevelSchema (field "name" string) (field "tracker" trackerSchemaDecoder)

type alias TrackerTopLevelSchema =
    { name : String
    , tracker : TrackerSchema
    }
