module Main exposing (..)

import Base64.Decode as Base64D
import Base64.Encode as Base64E
import Browser
import Debug as Debug
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, h2, hr, i, input, pre, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, field, int, map, map4, string)
import Json.Encode as E
import Maybe exposing (Maybe)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import String exposing (String)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((<?>))
import Url.Parser.Query as QP


main =
    Browser.application
        { init = \flags url _ -> initApp flags url
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = viewWithTile
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        , subscriptions = \_ -> Sub.none
        }


initApp : E.Value -> Url -> ( Model, Cmd Msg )
initApp _ url =
    let
        decode qp =
            case UrlParser.parse (UrlParser.top <?> QP.string qp) { url | path = "/" } of
                Just (Just encodedSchema) ->
                    case Base64D.decode Base64D.string encodedSchema of
                        Ok v ->
                            Just v

                        Err _ ->
                            Nothing

                _ ->
                    Nothing
    in
    case ( decode "edit", decode "track" ) of
        ( Just schema, _ ) ->
            Debug.log "edit existing" ( init url |> update (UpdateDefinition schema), Cmd.none )

        ( Nothing, Just schema ) ->
            case Decode.decodeString trackerTopLevelSchemaDecoder schema of
                Err e ->
                    Debug.log "bad track" ( { schemaJson = schema, url = url, state = InvalidDefinition e |> DefinitionStage }, Cmd.none )

                Ok d ->
                    ( { schemaJson = schema, url = url, state = (init url).state } |> update (MoveToPlayerSelection d), Cmd.none )

        _ ->
            ( init url, Cmd.none )



-- http://localhost:8000/src/Main.elm?track=abc
-- http://localhost:8000/src/Main.elm?edit=abc


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
    = ComponentIdNotFound CellScope String
    | CouldNotReadNumberOfPlayers
    | CouldNotParseWholeNumber String
    | TooManyPlayerGroupsDefined
    | CannotSetCurrentPlayerToThisWithEffectsOutsideContext


type Log
    = GameStarted Turns
    | ActionPerformed (Maybe Int) String (List Effect)
    | ValueUpdated (Maybe Int) String Value


type StageState
    = DefinitionStage DefinitionValidity
    | PlayerSelectionStage Int { minPlayers : Int, maxPlayers : Int } TrackerTopLevelSchema
    | BigError Error
    | TrackerStage TrackerTopLevelSchema TrackingState Turns (List Log)


type alias Stage =
    { schemaJson : String
    , state : StageState
    , url : Url
    }


type alias Model =
    Stage


init : Url -> Model
init url =
    { schemaJson = "", url = url, state = DefinitionStage StartingOut }


type TrackMsg
    = ApplyEffects (Maybe Int) String (List Effect)
    | SetWholeNumber String String (Maybe Int) String


type Msg
    = UpdateDefinition String
    | CreateTracker TrackerTopLevelSchema Turns
    | MoveToPlayerSelection TrackerTopLevelSchema
    | MoveToEdit
    | TestTracker String Turns
    | TrackerMsg TrackerTopLevelSchema TrackingState Turns TrackMsg
    | SetNumberOfPlayers TrackerTopLevelSchema String
    | ConfirmNumberOfPlayers TrackerTopLevelSchema
    | Noop


update : Msg -> Model -> Model
update msg model =
    let
        toState state =
            { schemaJson = model.schemaJson, url = model.url, state = state }

        withLogs schema state players newLogs =
            let
                oldLogs =
                    case model.state of
                        TrackerStage _ _ _ logs ->
                            logs

                        _ ->
                            []
            in
            TrackerStage schema state players (oldLogs ++ newLogs) |> toState
    in
    case msg of
        Noop ->
            model

        MoveToPlayerSelection def ->
            case findMinAndMaxPlayers def.tracker of
                [ players ] ->
                    if players.minPlayers == players.maxPlayers then
                        let
                            turns =
                                { currentPlayerTurn = 0, playerCount = players.maxPlayers }
                        in
                        withLogs def Dict.empty turns [ GameStarted turns ]

                    else
                        PlayerSelectionStage players.minPlayers players def |> toState

                [] ->
                    let
                        turns =
                            { currentPlayerTurn = 0, playerCount = 1 }
                    in
                    withLogs def Dict.empty turns [ GameStarted turns ]

                _ ->
                    BigError TooManyPlayerGroupsDefined |> toState

        UpdateDefinition def ->
            case Decode.decodeString trackerTopLevelSchemaDecoder def of
                Err e ->
                    { schemaJson = def, url = model.url, state = InvalidDefinition e |> DefinitionStage }

                Ok d ->
                    { schemaJson = def, url = model.url, state = ValidDefinition d |> DefinitionStage }

        MoveToEdit ->
            DefinitionStage StartingOut |> toState

        TestTracker def turns ->
            case Decode.decodeString trackerTopLevelSchemaDecoder def of
                Err e ->
                    { schemaJson = def, url = model.url, state = InvalidDefinition e |> DefinitionStage }

                Ok d ->
                    let
                        newModel =
                            withLogs d Dict.empty turns [ GameStarted turns ]
                    in
                    { newModel | schemaJson = def }

        CreateTracker def turns ->
            withLogs def Dict.empty turns [ GameStarted turns ]

        TrackerMsg schema state turns (ApplyEffects thisPlayer action effects) ->
            case List.foldl (applyEffect thisPlayer) (Ok ( schema, state, turns )) effects of
                Ok ( sc, st, ts ) ->
                    withLogs sc st ts [ ActionPerformed thisPlayer action effects ]

                Err e ->
                    BigError e |> toState

        TrackerMsg _ _ _ (SetWholeNumber _ _ _ "") ->
            model

        TrackerMsg schema state turns (SetWholeNumber id field player rawValue) ->
            case String.toInt rawValue of
                Just v ->
                    let
                        num =
                            WholeNumber v
                    in
                    withLogs schema (Dict.insert (key id player) num state) turns [ ValueUpdated player field num ]

                Nothing ->
                    CouldNotParseWholeNumber rawValue |> BigError |> toState

        ConfirmNumberOfPlayers schema ->
            case model.state of
                PlayerSelectionStage players bounds _ ->
                    let
                        turns =
                            { currentPlayerTurn = 0, playerCount = players }
                    in
                    withLogs schema Dict.empty turns [ GameStarted turns ]

                _ ->
                    BigError CouldNotReadNumberOfPlayers |> toState

        SetNumberOfPlayers _ "" ->
            model

        SetNumberOfPlayers schema n ->
            case ( String.toInt n, model.state ) of
                ( Just players, PlayerSelectionStage _ bounds _ ) ->
                    PlayerSelectionStage players bounds schema |> toState

                --                    TrackerStage schema Dict.empty { currentPlayerTurn = 0, playerCount = players } |> toState
                _ ->
                    BigError CouldNotReadNumberOfPlayers |> toState


applyEffect : Maybe Int -> Effect -> Result Error ( TrackerTopLevelSchema, TrackingState, Turns ) -> Result Error ( TrackerTopLevelSchema, TrackingState, Turns )
applyEffect thisPlayer eff result =
    case result of
        Err e ->
            Err e

        Ok ( schema, state, turns ) ->
            case eff of
                NextTurn ->
                    Ok ( schema, state, { turns | currentPlayerTurn = modBy turns.playerCount (turns.currentPlayerTurn + 1) } )

                SetCurrentPlayer CurrentIsThisPlayer ->
                    case thisPlayer of
                        Just player ->
                            Ok ( schema, state, { turns | currentPlayerTurn = player } )

                        Nothing ->
                            Err CannotSetCurrentPlayerToThisWithEffectsOutsideContext

                OnCells op targetId scope ->
                    let
                        scopedKeys =
                            case scope of
                                NonPlayer ->
                                    [ targetId ]

                                CurrentPlayer ->
                                    [ key targetId (Just turns.currentPlayerTurn) ]

                                AllPlayers ->
                                    List.range 0 (turns.playerCount - 1) |> List.map (\i -> key targetId (Just i))

                                ThisPlayer ->
                                    List.range 0 (turns.playerCount - 1) |> List.map (\i -> key targetId (Just i))
                    in
                    case idDefault targetId schema.tracker of
                        Just default ->
                            let
                                newValue currentValue =
                                    case ( op, currentValue ) of
                                        ( RestoreDefault, _ ) ->
                                            Ok default

                                        ( Adjust amount, WholeNumber v ) ->
                                            v + amount |> WholeNumber |> Ok

                                apply scopedKey res =
                                    res
                                        |> Result.andThen
                                            (\( sc, st, tu ) ->
                                                newValue (Maybe.withDefault default (Dict.get scopedKey st)) |> Result.map (\v -> ( sc, Dict.insert scopedKey v st, tu ))
                                            )
                            in
                            List.foldl apply (Ok ( schema, state, turns )) scopedKeys

                        Nothing ->
                            Err <| ComponentIdNotFound scope targetId


viewWithTile : Model -> Browser.Document Msg
viewWithTile model =
    { title =
        case model.state of
            DefinitionStage _ ->
                "b-b-r | Define Tracker"

            PlayerSelectionStage _ _ _ ->
                "b-b-r | Select Players"

            BigError _ ->
                "b-b-r | Error"

            TrackerStage schema _ _ _ ->
                "b-b-r | Track " ++ schema.name
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    case model.state of
        DefinitionStage valid ->
            viewEditTracker model.schemaJson model.url valid

        TrackerStage def state turns log ->
            div []
                [ viewTracker def state turns
                , viewLogs log
                , div [] [ makeUrl "track" model.schemaJson model.url ]
                , div [] [ makeUrl "edit" model.schemaJson model.url ]
                ]

        PlayerSelectionStage current players schema ->
            viewPlayerSelection current players schema

        BigError err ->
            div []
                [ h1 [] [ text "Error" ]
                , case err of
                    ComponentIdNotFound NonPlayer id ->
                        text ("Could not find non-player component ID when applying effect: " ++ id)

                    ComponentIdNotFound CurrentPlayer id ->
                        text ("Could not find component ID in player-group when applying effect: " ++ id)

                    ComponentIdNotFound ThisPlayer id ->
                        text ("Could not find component ID in player-group when applying effect: " ++ id)

                    ComponentIdNotFound AllPlayers id ->
                        text ("Could not find component ID in player-group when applying effect: " ++ id)

                    CouldNotReadNumberOfPlayers ->
                        text "Failed to read the number of players"

                    CouldNotParseWholeNumber raw ->
                        "Failed to read a numeric field as numeric, value: " ++ raw |> text

                    TooManyPlayerGroupsDefined ->
                        text "Too many player-groups are defined! Only one is allowed."

                    CannotSetCurrentPlayerToThisWithEffectsOutsideContext ->
                        text "To set this-player as the current player the effects must originate from the context of a player"
                , button [ onClick MoveToEdit ] [ text "Return to edit" ]
                ]


viewLogs : List Log -> Html Msg
viewLogs entries =
    div [] (h1 [] [ text "History" ] :: List.map viewLog entries)


viewLog : Log -> Html Msg
viewLog log =
    div []
        [ case log of
            GameStarted turns ->
                if turns.playerCount > 1 then
                    text (String.fromInt turns.playerCount ++ " player game started, player " ++ String.fromInt (turns.currentPlayerTurn + 1) ++ "'s turn")

                else
                    text "Game started"

            ActionPerformed (Just player) action _ ->
                text ("Player " ++ String.fromInt (player + 1) ++ " " ++ action)

            ActionPerformed Nothing action _ ->
                text action

            ValueUpdated Nothing thing value ->
                text (thing ++ " updated to " ++ valueToString value)

            ValueUpdated (Just player) thing value ->
                text ("Player " ++ String.fromInt (player + 1) ++ "'s " ++ thing ++ " updated to " ++ valueToString value)
        ]


viewPlayerSelection : Int -> { minPlayers : Int, maxPlayers : Int } -> TrackerTopLevelSchema -> Html Msg
viewPlayerSelection players { minPlayers, maxPlayers } schema =
    div []
        [ h1 [] [ text "Number of players" ]
        , input [ type_ "number", value (String.fromInt players), onInput (SetNumberOfPlayers schema), Html.Attributes.min (String.fromInt minPlayers), Html.Attributes.max (String.fromInt maxPlayers) ] []
        , button [ onClick (ConfirmNumberOfPlayers schema) ] [ text "Start game!" ]
        ]


makeUrl : String -> String -> Url -> Html Msg
makeUrl qp def url =
    a [ { url | query = qp ++ "=" ++ Url.percentEncode (Base64E.encode (Base64E.string def)) |> Just } |> Url.toString |> href ] [ "URL to " ++ qp |> text ]


viewEditTracker : String -> Url -> DefinitionValidity -> Html Msg
viewEditTracker def url valid =
    div []
        [ div [] [ h1 [] [ text "Edit Tracker" ] ]
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
        , div [] [ makeUrl "edit" def url ]
        , div [] [ h1 [] [ text "Test Tracker" ] ]
        , div [] [ button [ onClick (TestTracker simpleDominionTracker { currentPlayerTurn = 0, playerCount = 1 }) ] [ text "Dominion turn tracker" ] ]
        , div [] [ button [ onClick (TestTracker multiPlayerDominionTracker { currentPlayerTurn = 0, playerCount = 3 }) ] [ text "Multi-player Dominion turn tracker" ] ]
        , div [] [ button [ onClick (TestTracker killTeamTracker { currentPlayerTurn = 0, playerCount = 2 }) ] [ text "Kill team" ] ]
        ]


eval : TrackerTopLevelSchema -> Expression -> Maybe Int -> TrackingState -> Int -> Result String Value
eval schema expr thisPlayer state currentPlayer =
    let
        aux e =
            case e of
                Add op1 op2 ->
                    case ( aux op1, aux op2 ) of
                        ( Ok (WholeNumber a), Ok (WholeNumber b) ) ->
                            Ok (WholeNumber (a + b))

                        ( Err err, _ ) ->
                            Err err

                        ( _, Err err ) ->
                            Err err

                Ref targetId scope ->
                    let
                        player =
                            case ( scope, thisPlayer ) of
                                ( NonPlayer, _ ) ->
                                    Ok Nothing

                                ( CurrentPlayer, _ ) ->
                                    Ok (Just currentPlayer)

                                ( AllPlayers, _ ) ->
                                    Err "Calculated fields cannot reference all players"

                                ( ThisPlayer, Just this ) ->
                                    Ok (Just this)

                                ( ThisPlayer, Nothing ) ->
                                    Err "Referenced this player, but couldn't find them"
                    in
                    case idDefault targetId schema.tracker of
                        Just default ->
                            player |> Result.map (\p -> Maybe.withDefault default (Dict.get (key targetId p) state))

                        Nothing ->
                            Err ("Could not find identifier " ++ targetId ++ " anywhere")
    in
    aux expr


viewTrackerComponent : TrackerTopLevelSchema -> TrackerSchema -> TrackingState -> Turns -> Maybe Int -> Html TrackMsg
viewTrackerComponent schema tracker state turns playerNumber =
    case tracker of
        WholeNumberSchema s ->
            div []
                [ text s.text
                , text " "
                , if s.disabled then
                    text (valueToString <| Maybe.withDefault s.default (Dict.get (key s.id playerNumber) state))

                  else
                    input [ type_ "number", onInput (SetWholeNumber s.id s.text playerNumber), value (valueToString <| Maybe.withDefault s.default (Dict.get (key s.id playerNumber) state)) ] []
                ]

        Calculated s ->
            div []
                [ text s.text
                , text " "
                , case eval schema s.equals playerNumber state turns.currentPlayerTurn of
                    Ok (WholeNumber v) ->
                        v |> String.fromInt |> text

                    Err e ->
                        text ("Error: " ++ e)
                ]

        Group s ->
            div [] (List.map (\i -> viewTrackerComponent schema i state turns playerNumber) s.items)

        PlayerGroup s ->
            List.range 0 (turns.playerCount - 1) |> List.map (\i -> div [] [ h2 [] [ viewPlayerIndicator turns i ], viewTrackerComponent schema (Group { items = s.items }) state turns (Just i) ]) |> div []

        Action s ->
            button [ onClick (ApplyEffects playerNumber s.text s.effects) ] [ text s.text ]


viewPlayerIndicator : Turns -> Int -> Html TrackMsg
viewPlayerIndicator turns playerNumber =
    if turns.currentPlayerTurn == playerNumber then
        i [] [ "Player " ++ String.fromInt (playerNumber + 1) |> text ]

    else
        "Player " ++ String.fromInt (playerNumber + 1) |> text


viewTracker : TrackerTopLevelSchema -> TrackingState -> Turns -> Html Msg
viewTracker schema state turns =
    div []
        [ h1 [] [ text schema.name ]
        , Html.map (TrackerMsg schema state turns) <| viewTrackerComponent schema schema.tracker state turns Nothing
        ]



-- Tracker Schema


cellScopeDecoder : Maybe String -> Decoder CellScope
cellScopeDecoder scope =
    case scope of
        Nothing ->
            Decode.succeed NonPlayer

        Just "current-player" ->
            Decode.succeed CurrentPlayer

        Just "this-player" ->
            Decode.succeed ThisPlayer

        Just "all-players" ->
            Decode.succeed AllPlayers

        Just s ->
            Decode.fail (s ++ " is not a valid effect scope")


specificNewCurrentPlayerDecoder : String -> Decoder NewCurrentPlayer
specificNewCurrentPlayerDecoder newCp =
    case newCp of
        "this-player" ->
            Decode.succeed CurrentIsThisPlayer

        _ ->
            Decode.fail (newCp ++ " is not a valid new current player")


decodeNewCurrentPlayer : Decoder NewCurrentPlayer
decodeNewCurrentPlayer =
    string |> Decode.andThen specificNewCurrentPlayerDecoder


specificEffectDecoder : String -> Decoder Effect
specificEffectDecoder ty =
    case ty of
        "next-turn" ->
            Decode.succeed NextTurn

        "set-current-player" ->
            Decode.map SetCurrentPlayer (field "target" decodeNewCurrentPlayer)

        "restore-default" ->
            Decode.map2 (OnCells RestoreDefault) (field "targetId" string) (Decode.maybe (field "scope" string) |> Decode.andThen cellScopeDecoder)

        "adjust" ->
            Decode.map3 (\a -> OnCells (Adjust a)) (field "amount" Decode.int) (field "targetId" string) (Decode.maybe (field "scope" string) |> Decode.andThen cellScopeDecoder)

        _ ->
            Decode.fail (ty ++ " is not a valid effect type")


effectDecoder : Decoder Effect
effectDecoder =
    field "type" string |> Decode.andThen specificEffectDecoder


type CellEffect
    = RestoreDefault
    | Adjust Int


type CellScope
    = NonPlayer
    | CurrentPlayer
    | AllPlayers
    | ThisPlayer


type NewCurrentPlayer
    = CurrentIsThisPlayer


type Effect
    = OnCells CellEffect String CellScope
    | NextTurn
    | SetCurrentPlayer NewCurrentPlayer


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


addDecoder : Decoder Expression
addDecoder =
    Decode.map2 Add (field "op1" expressionDecoder) (field "op2" expressionDecoder)


refDecoder : Decoder Expression
refDecoder =
    Decode.map2 Ref (field "targetId" string) (Decode.maybe (field "scope" string) |> Decode.andThen cellScopeDecoder)


specificExpressionDecoder : String -> Decoder Expression
specificExpressionDecoder ty =
    case ty of
        "add" ->
            addDecoder

        "ref" ->
            refDecoder

        _ ->
            Decode.fail (ty ++ " is not a valid expression type")


expressionDecoder : Decoder Expression
expressionDecoder =
    field "type" string |> Decode.andThen specificExpressionDecoder


wholeNumberDecoder : Decoder TrackerSchema
wholeNumberDecoder =
    Decode.map4
        (\text default id disabled -> WholeNumberSchema { text = text, default = default, id = id, disabled = disabled == Just True })
        (field "text" string)
        (field "default" (Decode.map WholeNumber Decode.int))
        (field "id" string)
        (Decode.maybe (field "disabled" Decode.bool))


calculatedDecoder : Decoder TrackerSchema
calculatedDecoder =
    Decode.map2
        (\text equals -> Calculated { text = text, equals = equals })
        (field "text" string)
        (field "equals" expressionDecoder)


specificTrackerSchemaDecoder : String -> Decoder TrackerSchema
specificTrackerSchemaDecoder ty =
    case ty of
        "player-group" ->
            playerGroupDecoder

        "group" ->
            groupDecoder

        "action" ->
            actionDecoder

        "number" ->
            wholeNumberDecoder

        "calculated" ->
            calculatedDecoder

        _ ->
            Decode.fail (ty ++ " is not a valid tracker component type")


trackerSchemaDecoder : Decoder TrackerSchema
trackerSchemaDecoder =
    field "type" string |> Decode.andThen specificTrackerSchemaDecoder


type Expression
    = Add Expression Expression
    | Ref String CellScope


type TrackerSchema
    = PlayerGroup { items : List TrackerSchema, minPlayers : Int, maxPlayers : Int }
    | Group { items : List TrackerSchema }
    | Action { text : String, effects : List Effect }
    | WholeNumberSchema { text : String, default : Value, id : String, disabled : Bool }
    | Calculated { text : String, equals : Expression }


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

        Calculated _ ->
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

        Calculated _ ->
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


killTeamTracker : String
killTeamTracker =
    """
{
  "name": "Kill Team Tracker",
  "tracker": {
    "type": "group",
    "items": [
      {
        "type": "action",
        "text": "Next Turning Point",
        "effects": [
          {
            "type": "adjust",
            "amount": 1,
            "targetId": "turning-point"
          },
          {
            "type": "adjust",
            "amount": 1,
            "targetId": "command-points",
            "scope": "all-players"
          }
        ]
      },
      {
        "type": "number",
        "text": "Turning Point",
        "default": 1,
        "disabled": true,
        "id": "turning-point"
      },
      {
        "type": "player-group",
        "minPlayers": 2,
        "maxPlayers": 2,
        "items": [
          {
            "type": "action",
            "text": "Has Initiative",
            "effects": [
              {
                "type": "set-current-player",
                "target": "this-player"
              }
            ]
          },
          {
            "type": "number",
            "text": "Command Points",
            "default": 2,
            "id": "command-points"
          },
          {
            "type": "number",
            "text": "Primary VP",
            "default": 0,
            "id": "primary-vp"
          },
          {
            "type": "number",
            "text": "Secondary VP",
            "default": 0,
            "id": "secondary-vp"
          },
          {
            "type": "calculated",
            "text": "Total VP",
            "equals": {
              "type": "add",
              "op1": {
                "type": "ref",
                "targetId": "primary-vp",
                "scope": "this-player"
              },
              "op2": {
                "type": "ref",
                "targetId": "secondary-vp",
                "scope": "this-player"
              }
            }
          }
        ]
      }
    ]
  }
}
"""
