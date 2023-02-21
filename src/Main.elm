module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, input, pre, text, textarea, i)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, field, int, map, map4, string)
import Maybe exposing (Maybe)
import String exposing (String)
import Debug as Debug

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



-- Key for id and perhaps player


key : String -> Maybe Int -> String
key id player =
    case player of
        Just p ->
            "player_" ++ String.fromInt p ++ "_" ++ id

        Nothing ->
            id


type alias Turns =
    { currentPlayerTurn : Int
    , playerCount : Int
    }


type Error
    = ComponentIdNotFound String
    | PlayerComponentIdNotFound String
    | CouldNotReadNumberOfPlayers
    | CouldNotParseWholeNumber String
    | TooManyPlayerGroupsDefined


type StageState
    = DefinitionStage DefinitionValidity
    | PlayerSelectionStage Int { minPlayers : Int, maxPlayers : Int } TrackerTopLevelSchema
    | TrackerStage TrackerTopLevelSchema TrackingState Turns
    | BigError Error


type alias Stage =
    { schemaJson : String
    , state : StageState
    }


type alias Model =
    Stage


init : Model
init =
    { schemaJson = "", state = DefinitionStage StartingOut }


type TrackMsg
    = ApplyEffects (List Effect)
    | SetWholeNumber String (Maybe Int) String


type Msg
    = UpdateDefinition String
    | CreateTracker TrackerTopLevelSchema Turns
    | MoveToPlayerSelection TrackerTopLevelSchema
    | MoveToEdit
    | TestTracker String Turns
    | TrackerMsg TrackerTopLevelSchema TrackingState Turns TrackMsg
    | SetNumberOfPlayers TrackerTopLevelSchema String


update : Msg -> Model -> Model
update msg model =
    let
        toState state =
            { schemaJson = model.schemaJson, state = state }
    in
    case msg of
        MoveToPlayerSelection def ->
            case findMinAndMaxPlayers def.tracker of
                [ players ] ->
                    PlayerSelectionStage 1 players def |> toState

                [] ->
                    TrackerStage def Dict.empty { currentPlayerTurn = 0, playerCount = 1 } |> toState

                _ ->
                    BigError TooManyPlayerGroupsDefined |> toState

        UpdateDefinition def ->
            case Decode.decodeString trackerTopLevelSchemaDecoder def of
                Err e ->
                    { schemaJson = def, state = InvalidDefinition e |> DefinitionStage }

                Ok d ->
                    { schemaJson = def, state = ValidDefinition d |> DefinitionStage }

        MoveToEdit ->
            DefinitionStage StartingOut |> toState

        TestTracker def turns ->
            case Decode.decodeString trackerTopLevelSchemaDecoder def of
                Err e ->
                    { schemaJson = def, state = InvalidDefinition e |> DefinitionStage }

                Ok d ->
                    { schemaJson = def, state = TrackerStage d Dict.empty turns }

        CreateTracker def turns ->
            TrackerStage def Dict.empty turns |> toState

        TrackerMsg schema state turns (ApplyEffects effects) ->
            case List.foldl applyEffect (Ok ( schema, state, turns )) effects of
                Ok ( sc, st, ts ) ->
                    TrackerStage sc st ts |> toState

                Err e ->
                    BigError e |> toState

        TrackerMsg schema state turns (SetWholeNumber id player rawValue) ->
            case String.toInt rawValue of
                Just v ->
                    TrackerStage schema (Dict.insert (key id player) (WholeNumber v) state) turns |> toState

                Nothing ->
                    CouldNotParseWholeNumber rawValue |> BigError |> toState

        SetNumberOfPlayers schema n ->
            case String.toInt n of
                Just players ->
                    TrackerStage schema Dict.empty { currentPlayerTurn = 0, playerCount = players } |> toState

                Nothing ->
                    BigError CouldNotReadNumberOfPlayers |> toState


applyEffect : Effect -> Result Error ( TrackerTopLevelSchema, TrackingState, Turns ) -> Result Error ( TrackerTopLevelSchema, TrackingState, Turns )
applyEffect eff result =
    case result of
        Err e ->
            Err e

        Ok ( schema, state, turns ) ->
            case eff of
                NextTurn ->
                    Ok ( schema, state, { turns | currentPlayerTurn = modBy turns.playerCount (turns.currentPlayerTurn + 1) } )

                RestoreDefault { targetId } ->
                    case idDefault targetId schema.tracker of
                        Just default ->
                            Ok <| ( schema, Dict.insert targetId default state, turns )

                        Nothing ->
                            Err <| ComponentIdNotFound targetId

                RestoreCurrentPlayerDefault { targetId } ->
                    case idDefault targetId schema.tracker of
                        Just default ->
                            Ok <| ( schema, Dict.insert (key targetId (Just turns.currentPlayerTurn)) default state, turns )

                        Nothing ->
                            Err <| PlayerComponentIdNotFound targetId


view : Model -> Html Msg
view model =
    case model.state of
        DefinitionStage valid ->
            viewEditTracker model.schemaJson valid

        TrackerStage def state turns ->
            viewTracker def state turns

        PlayerSelectionStage current players schema ->
            viewPlayerSelection current players schema

        BigError err ->
            div []
                [ h1 [] [ text "Error" ]
                , case err of
                    ComponentIdNotFound id ->
                        text ("Could not find global component ID when applying effect: " ++ id)

                    PlayerComponentIdNotFound id ->
                        text ("Could not find component ID in player-group when applying effect: " ++ id)

                    CouldNotReadNumberOfPlayers ->
                        text "Failed to read the number of players"

                    CouldNotParseWholeNumber raw ->
                        "Failed to read a numeric field as numeric, value: " ++ raw |> text

                    TooManyPlayerGroupsDefined ->
                        text "Too many player-groups are defined! Only one is allowed."
                , button [ onClick MoveToEdit ] [ text "Return to edit" ]
                ]


viewPlayerSelection : Int -> { minPlayers : Int, maxPlayers : Int } -> TrackerTopLevelSchema -> Html Msg
viewPlayerSelection players { minPlayers, maxPlayers } schema =
    div []
        [ h1 [] [ text "Number of players" ]
        , input [ type_ "number", value (String.fromInt players), onInput (SetNumberOfPlayers schema), Html.Attributes.min (String.fromInt minPlayers), Html.Attributes.max (String.fromInt maxPlayers) ] []
        ]


viewEditTracker : String -> DefinitionValidity -> Html Msg
viewEditTracker def valid =
    div []
        [ div [] [ h1 [] [ text "New Tracker" ] ]
        , div [] [ textarea [ cols 40, rows 10, placeholder "...", onInput UpdateDefinition, value def ] [] ]
        , case valid of
            ValidDefinition d ->
                div []
                    [ div [] [ text <| "Definition good: " ++ d.name ]
                    , button [ onClick (MoveToPlayerSelection d) ] [ text "Create!" ]
                    ]

            InvalidDefinition err ->
                div []
                    [ text "Definition invalid"
                    , pre [] [ text <| Decode.errorToString err ]
                    ]

            StartingOut ->
                text ""
        , div [] [ h1 [] [ text "Test Tracker" ] ]
        , div [] [ button [ onClick (TestTracker simpleDominionTracker { currentPlayerTurn = 0, playerCount = 1 }) ] [ text "Dominion turn tracker" ] ]
        , div [] [ button [ onClick (TestTracker multiPlayerDominionTracker { currentPlayerTurn = 0, playerCount = 3 }) ] [ text "Multi-player Dominion turn tracker" ] ]
        ]


viewTrackerComponent : TrackerSchema -> TrackingState -> Turns -> Maybe Int -> Html TrackMsg
viewTrackerComponent tracker state turns playerNumber =
    case tracker of
        WholeNumberSchema s ->
            div [] [ text s.text, input [ type_ "number", onInput (SetWholeNumber s.id playerNumber), value (valueToString <| Maybe.withDefault s.default (Dict.get (key s.id playerNumber) state)) ] [] ]

        Group s ->
            div [] (List.map (\i -> viewTrackerComponent i state turns playerNumber) s.items)

        PlayerGroup s ->
            List.range 0 (turns.playerCount - 1) |> List.map (\i -> div [] [ h2 [] [ viewPlayerIndicator turns i ], viewTrackerComponent (Group { items = s.items }) state turns (Just i) ]) |> div []

        Action s ->
            button [ onClick (ApplyEffects s.effects) ] [ text s.text ]

viewPlayerIndicator : Turns -> Int -> Html TrackMsg
viewPlayerIndicator turns playerNumber =
    if turns.currentPlayerTurn == playerNumber
    then i [] ["Player " ++ String.fromInt ( playerNumber + 1) |> text]
    else "Player " ++ String.fromInt (playerNumber + 1) |> text


viewTracker : TrackerTopLevelSchema -> TrackingState -> Turns -> Html Msg
viewTracker schema state turns =
    div []
        [ h1 [] [ text schema.name ]
        , Html.map (TrackerMsg schema state turns) <| viewTrackerComponent schema.tracker state turns Nothing
        ]



-- Tracker Schema


restoreCurrentPlayerDefaultDecoder : Decoder Effect
restoreCurrentPlayerDefaultDecoder =
    Decode.map2 (\targetId _ -> RestoreCurrentPlayerDefault  { targetId = targetId }) (field "targetId" string) (field "scope" string)


restoreDefaultDecoder : Decoder Effect
restoreDefaultDecoder =
    Decode.map (\targetId -> RestoreDefault { targetId = targetId }) (field "targetId" string) 


specificEffectDecoder : String -> Decoder Effect
specificEffectDecoder ty =
    case ty of
        "next-turn" ->
            Decode.succeed NextTurn

        "restore-default" ->
            Decode.lazy <| \_ -> Decode.oneOf [ restoreCurrentPlayerDefaultDecoder, restoreDefaultDecoder ]

        _ ->
            Decode.fail (ty ++ " is not a valid effect type")


effectDecoder : Decoder Effect
effectDecoder =
    field "type" string |> Decode.andThen specificEffectDecoder


type Effect
    = RestoreDefault { targetId : String }
    | RestoreCurrentPlayerDefault { targetId : String }
    | NextTurn


playerGroupDecoder : Decoder TrackerSchema
playerGroupDecoder =
    Decode.map3 (\items minPlayers maxPlayers -> PlayerGroup { items = items, minPlayers = minPlayers, maxPlayers = maxPlayers }) (field "items" (Decode.list trackerSchemaDecoder)) (field "minPlayers" Decode.int) (field "maxPlayers" Decode.int)


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
    Decode.lazy <| \_ -> Decode.oneOf [ playerGroupDecoder, groupDecoder, actionDecoder, wholeNumberDecoder ]


type TrackerSchema
    = PlayerGroup { items : List TrackerSchema, minPlayers : Int, maxPlayers : Int }
    | Group { items : List TrackerSchema }
    | Action { text : String, effects : List Effect }
    | WholeNumberSchema { text : String, default : Value, id : String }


findMinAndMaxPlayers : TrackerSchema -> List { minPlayers : Int, maxPlayers : Int }
findMinAndMaxPlayers schema =
    case schema of
        WholeNumberSchema s ->
            []

        Group s ->
            List.concatMap findMinAndMaxPlayers s.items

        PlayerGroup s ->
            { minPlayers = s.minPlayers, maxPlayers = s.maxPlayers } :: List.concatMap findMinAndMaxPlayers s.items

        Action _ ->
            []


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

        PlayerGroup s ->
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
            "type": "restore-default",
            "targetId": "a"
          },
          {
            "type": "restore-default",
            "targetId": "b"
          },
          {
            "type": "restore-default",
            "targetId": "em"
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


multiPlayerDominionTracker : String
multiPlayerDominionTracker =
    """
{
  "name": "Multi-player Dominion Turn Tracker",
  "tracker": {
    "type": "group",
    "items": [
      {
        "type": "action",
        "text": "Next Turn",
        "effects": [
          {
            "type": "next-turn"
          },
          {
            "type": "restore-default",
            "targetId": "a",
            "scope": "current-player"
          },
          {
            "type": "restore-default",
            "targetId": "b",
            "scope": "current-player"
          },
          {
            "type": "restore-default",
            "targetId": "em",
            "scope": "current-player"
          }
        ]
      },
      {
        "type": "player-group",
        "minPlayers": 2,
        "maxPlayers": 4,
        "items": [
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
    ]
  }
}
              """
