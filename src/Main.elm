module Main exposing (..)

import Validation as Validation
import Base64.Decode as Base64D
import Base64.Encode as Base64E
import Browser
import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, a, b, button, details, div, h1, h2, hr, i, input, pre, summary, text, textarea, span, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Json.Encode as E
import Maybe exposing (Maybe)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import Round as Round
import String exposing (String)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((<?>))
import Url.Parser.Query as QP

import TrackerSchema exposing (..)


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
            ( init url |> update (UpdateDefinition schema), Cmd.none )

        ( Nothing, Just schema ) ->
            case Decode.decodeString trackerTopLevelSchemaDecoder schema of
                Err e ->
                    ( { schemaJson = schema, url = url, state = CannotDecode e |> DefinitionStage }, Cmd.none )

                Ok d ->
                    ( { schemaJson = schema, url = url, state = (init url).state } |> update (MoveToPlayerSelection d), Cmd.none )

        _ ->
            ( init url, Cmd.none )

type DefinitionValidity
    = CanDecodeWithoutErrors TrackerTopLevelSchema
    | CanDecodeWithErrors (List Validation.BadSchemaError)
    | CannotDecode Decode.Error
    | StartingOut

type KeyMaybeUnderList v
    = KeyUnderList Int v
    | KeyNotUnderList v

type Key v
    = NonPlayerKey (KeyMaybeUnderList v)
    | PlayerKey Int (KeyMaybeUnderList v)

htmlIdFromKey : String -> Key String -> Html.Attribute a
htmlIdFromKey context key =
    let
      idify = String.replace " " "-"
      idFromMk mk =
          case mk of
            KeyUnderList i v -> "item-" ++ String.fromInt i ++ "-" ++ idify v ++ "-" ++ idify context
            KeyNotUnderList v -> idify v ++ "-" ++ idify context
    in
      id <| case key of
        NonPlayerKey mk -> idFromMk mk
        PlayerKey p mk -> "player-" ++ String.fromInt p ++ "-" ++ idFromMk mk

emptyKey : Key ()
emptyKey =
    () |> KeyNotUnderList |> NonPlayerKey

keyWithPlayerNumber : Int -> Key a -> Key a
keyWithPlayerNumber player key =
    case key of
        NonPlayerKey mul ->
            PlayerKey player mul

        PlayerKey _ mul ->
            PlayerKey player mul


keyWithItemNumber : Int -> Key a -> Key a
keyWithItemNumber item key =
    let
        underList mul =
            case mul of
                KeyUnderList _ v ->
                    KeyUnderList item v

                KeyNotUnderList v ->
                    KeyUnderList item v
    in
    case key of
        NonPlayerKey mul ->
            mul |> underList |> NonPlayerKey

        PlayerKey player mul ->
            mul |> underList |> PlayerKey player


keyWithId : String -> Key a -> Key String
keyWithId id key =
    let
        withId mul =
            case mul of
                KeyUnderList l _ ->
                    KeyUnderList l id

                KeyNotUnderList _ ->
                    KeyNotUnderList id
    in
    case key of
        NonPlayerKey mul ->
            mul |> withId |> NonPlayerKey

        PlayerKey player mul ->
            mul |> withId |> PlayerKey player


schemaId : Key a -> a
schemaId key =
    let
        idUnderList mul =
            case mul of
                KeyUnderList _ id ->
                    id

                KeyNotUnderList id ->
                    id
    in
    case key of
        NonPlayerKey mul ->
            idUnderList mul

        PlayerKey _ mul ->
            idUnderList mul


type alias ListItem = { index : Int, live : Bool, text : String }

type alias MaybeUnderList =
    { notUnderList : Dict String Value
    , underList : Dict Int (Dict String Value)
    , listItems : Array ListItem
    }


listItems : Key String -> TrackingState -> Array ListItem
listItems key state =
    case key of
        NonPlayerKey _ ->
            state.nonPlayer.listItems

        PlayerKey p _ ->
            state.player
                |> Dict.get p
                |> Maybe.map .listItems
                |> Maybe.withDefault Array.empty


type alias TrackingState =
    { nonPlayer : MaybeUnderList
    , player : Dict Int MaybeUnderList
    }

arrayUpdate : Int -> (a -> a) -> Array a -> Array a
arrayUpdate idx f vs =
    case Array.get idx vs of
        Just v -> Array.set idx (f v) vs
        Nothing -> vs

setItemText : Key () -> String -> TrackingState -> TrackingState
setItemText key text state =
    let
        setText item = {item | text = text}
    in
    case key of
        PlayerKey p (KeyUnderList idx _) ->
            {
              state |
              player = Dict.update p (Maybe.withDefault emptyMul >> (\s -> {s | listItems=arrayUpdate idx setText s.listItems }) >> Just) state.player
            }
        NonPlayerKey (KeyUnderList idx _) ->
            let np = state.nonPlayer
            in
              {state | nonPlayer={ np | listItems=arrayUpdate idx setText np.listItems } }
        PlayerKey _ (KeyNotUnderList _) -> state
        NonPlayerKey (KeyNotUnderList _) -> state

getItemText : Key a -> TrackingState -> Maybe String
getItemText key state =
    case key of
        PlayerKey p (KeyUnderList idx _) ->
            state.player |> Dict.get p |> Maybe.map .listItems |> Maybe.andThen (Array.get idx) |> Maybe.map .text
        NonPlayerKey (KeyUnderList idx _) ->
            state.nonPlayer.listItems |> Array.get idx |> Maybe.map .text
        PlayerKey _ (KeyNotUnderList _) -> Nothing
        NonPlayerKey (KeyNotUnderList _) -> Nothing

emptyMul =
    { notUnderList = Dict.empty
    , underList = Dict.empty
    , listItems = Array.empty
    }


emptyState : TrackingState
emptyState =
    { nonPlayer =
        { notUnderList = Dict.empty
        , underList = Dict.empty
        , listItems = Array.empty
        }
    , player = Dict.empty
    }


modItem : (ListItem -> ListItem) -> Key k -> TrackingState -> TrackingState
modItem f key state =
    let
        mod idx mul =
            case mul of
                Just m ->
                    { m | listItems = Array.map (\item -> if item.index == idx then f item else item) m.listItems }

                Nothing ->
                    emptyMul
    in
    case key of
        NonPlayerKey (KeyUnderList idx _) ->
            { state | nonPlayer = mod idx (Just state.nonPlayer) }

        PlayerKey p (KeyUnderList idx _) ->
            { state | player = Dict.update p (mod idx >> Just) state.player }

        NonPlayerKey (KeyNotUnderList _) ->
            state

        PlayerKey _ (KeyNotUnderList _) ->
            state

resurrectItem : Key k -> TrackingState -> TrackingState
resurrectItem = modItem (\item -> { item | live = True })

removeItem : Key k -> TrackingState -> TrackingState
removeItem = modItem (\item -> { item | live = False })

deleteLastItem : Key k -> TrackingState -> TrackingState
deleteLastItem = modItems (Array.slice 0 -1)

modItems : (Array ListItem -> Array ListItem) -> Key k -> TrackingState -> TrackingState
modItems f key state =
    let
        mod mul =
            case mul of
                Just m ->
                    { m | listItems = f m.listItems }

                Nothing ->
                    { emptyMul | listItems = f Array.empty }
    in
    case key of
        NonPlayerKey (KeyUnderList _ _) ->
            { state | nonPlayer = mod (Just state.nonPlayer) }

        PlayerKey p (KeyUnderList _ _) ->
            { state | player = Dict.update p (mod >> Just) state.player }

        NonPlayerKey (KeyNotUnderList _) ->
            { state | nonPlayer = mod (Just state.nonPlayer) }

        PlayerKey p (KeyNotUnderList _) ->
            { state | player = Dict.update p (mod >> Just) state.player }


addItem : Key k -> TrackingState -> TrackingState
addItem =
  modItems (\items -> Array.push { live = True, index = Array.length items, text = "Item " ++ String.fromInt (Array.length items + 1) } items)

set : Key String -> Value -> TrackingState -> TrackingState
set key value state =
    let
        setInner id dict =
            case dict of
                Nothing ->
                    Dict.singleton id value

                Just d ->
                    Dict.insert id value d

        setMul mulKey mul =
            case ( mulKey, mul ) of
                ( KeyNotUnderList id, Nothing ) ->
                    { listItems = Array.empty, underList = Dict.empty, notUnderList = Dict.singleton id value }

                ( KeyNotUnderList id, Just s ) ->
                    { s | notUnderList = Dict.insert id value s.notUnderList }

                ( KeyUnderList idx id, Nothing ) ->
                    { listItems = Array.empty, notUnderList = Dict.empty, underList = Dict.singleton idx (Dict.singleton id value) }

                ( KeyUnderList idx id, Just s ) ->
                    { s | underList = Dict.update idx (setInner id >> Just) s.underList }
    in
    case key of
        NonPlayerKey mulKey ->
            { state | nonPlayer = setMul mulKey (Just state.nonPlayer) }

        PlayerKey p mulKey ->
            { state | player = Dict.update p (setMul mulKey >> Just) state.player }


get : Key String -> ( TrackingState, TrackerTopLevelSchema ) -> Result String Value
get key ( state, schema ) =
    case idDefault (schemaId key) schema.tracker of
        Nothing ->
            "Could not find default for ID " ++ schemaId key |> Err

        Just default ->
            let
                lookupMul mulKey mul =
                    case mulKey of
                        KeyNotUnderList id ->
                            Dict.get id mul.notUnderList

                        KeyUnderList idx id ->
                            Dict.get idx mul.underList |> Maybe.andThen (Dict.get id)

                current =
                    case key of
                        NonPlayerKey mulKey ->
                            lookupMul mulKey state.nonPlayer

                        PlayerKey player mulKey ->
                            case Dict.get player state.player of
                                Nothing ->
                                    Nothing

                                Just next ->
                                    lookupMul mulKey next
            in
            current
                |> Maybe.withDefault (lookupDefault default key)
                |> Ok


playerIds : Turns -> List Int
playerIds turns =
    List.range 0 (turns.playerCount - 1)


type alias Turns =
    { currentPlayerTurn : Int
    , playerCount : Int
    , disabled : Bool
    }


type Error
    = ComponentIdNotFound CellScope String
    | CouldNotReadNumberOfPlayers
    | CouldNotParseWholeNumber String
    | TooManyPlayerGroupsDefined
    | CannotSetCurrentPlayerToThisWithEffectsOutsideContext
    | UnexpectedError String


type alias Field a =
    { key : Key a, text : String }


type alias PlayerAliases =
    Dict Int String


type StageState
    = DefinitionStage DefinitionValidity
    | PlayerSelectionStage Int PlayerGroupData TrackerTopLevelSchema PlayerAliases
    | BigError Model Error
    | TrackerStage TrackerTopLevelSchema TrackingState Turns (List Log) PlayerAliases


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

type Log
    = GameStarted Turns
    | ActionPerformed (Key ()) String (List Log)
    | ValueUpdated { old : Value, new : Value, field : Field String }
    | CurrentPlayerChanged { old : Int, new : Int }
    | ListItemAdded (Field ())
    | ListItemRemoved (Field ())

type TrackMsg
    = ApplyEffects (Key ()) String (List Effect)
    | SetWholeNumber (Field String) String
    | SetItemText (Key ()) String
    | UpdatePlayerAlias Int String
    | NewListItem (Field ())
    | RemoveListItem (Field ())
    | UndoLastLog
    | Undo Log

type Msg
    = UpdateDefinition String
    | MoveToPlayerSelection TrackerTopLevelSchema
    | MoveToEdit
    | MoveTo Model
    | TrackerMsg TrackerTopLevelSchema TrackingState Turns PlayerAliases TrackMsg
    | SetNumberOfPlayers TrackerTopLevelSchema String
    | ConfirmNumberOfPlayers TrackerTopLevelSchema
    | Noop


update : Msg -> Model -> Model
update msg model =
    let
        toState state =
            { schemaJson = model.schemaJson, url = model.url, state = state }

        unlog schema state turns aliases =
            let
                (finalLog, newLogs) =
                    case model.state of
                        TrackerStage _ _ _ logs _ ->
                            let
                              dropped = List.take (List.length logs - 1) logs
                            in 
                              case List.drop (List.length logs - 1) logs of
                                 [l] -> (Just l, dropped)
                                 _ -> (Nothing, dropped)
                        _ -> (Nothing, [] )

            in
                (finalLog, TrackerStage schema state turns newLogs aliases |> toState )

        log schema state turns newLog aliases =
            let
                oldLogs =
                    case model.state of
                        TrackerStage _ _ _ logs _ ->
                            logs

                        _ ->
                            []

                newLogs =
                    case newLog of
                        Just l ->
                            oldLogs ++ [ l ]

                        Nothing ->
                            oldLogs

                compacted =
                    if List.length newLogs <= 1 then
                        newLogs

                    else
                        let
                            head =
                                List.take (List.length newLogs - 2) newLogs

                            tail =
                                List.drop (List.length newLogs - 2) newLogs

                            newTail =
                                case tail of
                                    [ ValueUpdated a, ValueUpdated b ] ->
                                        if a.field == b.field then
                                            if b.new == a.old then
                                                []

                                            else
                                                [ ValueUpdated { b | old = a.old } ]

                                        else
                                            tail

                                    _ ->
                                        tail
                        in
                        head ++ newTail
            in
            TrackerStage schema state turns compacted aliases |> toState
    in
    case msg of
        MoveTo m ->
            m

        Noop ->
            model

        MoveToPlayerSelection def ->
            case findPlayerGroup def.tracker of
                [ ( players, aliases ) ] ->
                    if players.minPlayers == players.maxPlayers then
                        let
                            turns =
                                { currentPlayerTurn = 0, playerCount = players.maxPlayers, disabled = not def.turns }
                        in
                        log def emptyState turns (Just <| GameStarted turns) aliases

                    else
                        PlayerSelectionStage players.minPlayers players def aliases |> toState

                [] ->
                    let
                        turns =
                            { currentPlayerTurn = 0, playerCount = 1, disabled = not def.turns }
                    in
                    log def emptyState turns (Just <| GameStarted turns) Dict.empty

                _ ->
                    BigError model TooManyPlayerGroupsDefined |> toState

        UpdateDefinition def ->
            case Decode.decodeString trackerTopLevelSchemaDecoder def of
                Err e ->
                    { schemaJson = def, url = model.url, state = CannotDecode e |> DefinitionStage }

                Ok d ->
                    case (Validation.validateSchema d).errors of
                        [] -> { schemaJson = def, url = model.url, state = CanDecodeWithoutErrors d |> DefinitionStage }
                        errors -> { schemaJson = def, url = model.url, state = CanDecodeWithErrors errors |> DefinitionStage }

        MoveToEdit ->
            DefinitionStage StartingOut |> toState

        TrackerMsg schema state turns aliases (ApplyEffects key action effects) ->
            case List.foldl (applyEffect key schema) (Ok (state, turns, [])) effects of
                Ok (st, ts, appliedEffects) ->
                    log schema st ts (Just <| ActionPerformed key action appliedEffects) aliases

                Err e ->
                    BigError model e |> toState

        TrackerMsg schema state turns aliases (UpdatePlayerAlias playerNumber newAlias) ->
            log schema state turns Nothing (Dict.insert playerNumber newAlias aliases)

        TrackerMsg schema state turns aliases (Undo entry) ->
            let
                undo l (st, ts) =
                  case l of
                    GameStarted _ ->
                        (st, ts)
                    ActionPerformed k a logs ->
                        logs
                            |> List.reverse
                            |> List.foldl undo (st, ts)
                    ValueUpdated { old, field } ->
                        (set field.key old st, ts)
                    CurrentPlayerChanged { old } ->
                        (st, { ts | currentPlayerTurn = old })
                    ListItemAdded {key} ->
                        (deleteLastItem key st, ts)
                    ListItemRemoved {key} ->
                        (resurrectItem key st, ts)
            in
              case undo entry (state, turns) of
                 (st, ts) -> log schema st ts Nothing aliases

        TrackerMsg schema state turns aliases UndoLastLog ->
            case unlog schema state turns aliases of
                (Nothing, m) -> m
                (Just l, m) -> update (TrackerMsg schema state turns aliases (Undo l)) m

        TrackerMsg _ _ _ _ (SetWholeNumber _ "") ->
            model

        TrackerMsg schema state turns aliases (SetItemText key text) ->
            log schema (setItemText key text state) turns Nothing aliases

        TrackerMsg schema state turns aliases (NewListItem field) ->
            log schema (addItem field.key state) turns (ListItemAdded field |> Just) aliases

        TrackerMsg schema state turns aliases (RemoveListItem field) ->
            log schema (removeItem field.key state) turns (ListItemRemoved field |> Just) aliases

        TrackerMsg schema state turns aliases (SetWholeNumber field rawValue) ->
            case String.toInt rawValue of
                Just v ->
                    let
                        num =
                            WholeNumber v
                    in
                    case get field.key ( state, schema ) of
                        Ok oldValue ->
                            let
                                event =
                                    ValueUpdated { old = oldValue, new = num, field = field }
                            in
                            if oldValue == num then
                                model

                            else
                                log schema (set field.key num state) turns (Just <| event) aliases

                        Err e ->
                            e |> UnexpectedError |> BigError model |> toState

                Nothing ->
                    CouldNotParseWholeNumber rawValue |> BigError model |> toState

        ConfirmNumberOfPlayers schema ->
            case model.state of
                PlayerSelectionStage players bounds _ aliases ->
                    let
                        turns =
                            { currentPlayerTurn = 0, playerCount = players, disabled = not schema.turns }
                    in
                    log schema emptyState turns (Just <| GameStarted turns) aliases

                _ ->
                    BigError model CouldNotReadNumberOfPlayers |> toState

        SetNumberOfPlayers _ "" ->
            model

        SetNumberOfPlayers schema n ->
            case ( String.toInt n, model.state ) of
                ( Just players, PlayerSelectionStage _ bounds _ aliases ) ->
                    PlayerSelectionStage players bounds schema aliases |> toState

                _ ->
                    BigError model CouldNotReadNumberOfPlayers |> toState

applyEffect : Key a -> TrackerTopLevelSchema -> Effect -> Result Error (TrackingState, Turns, List Log) -> Result Error (TrackingState, Turns, List Log)
applyEffect key schema eff result =
    case result of
        Err e ->
            Err e

        Ok ( state, turns, logs ) ->
            let
                setCurrentPlayer player =
                  Ok ( state, { turns | currentPlayerTurn = player }, logs ++ [CurrentPlayerChanged { old = turns.currentPlayerTurn, new = player}] )
            in
            case eff of
                NextTurn ->
                    modBy turns.playerCount (turns.currentPlayerTurn + 1) |> setCurrentPlayer

                SetCurrentPlayer CurrentIsThisPlayer ->
                    case key of
                        PlayerKey player _ ->
                            setCurrentPlayer player

                        _ ->
                            Err CannotSetCurrentPlayerToThisWithEffectsOutsideContext

                OnCells op targetId scope ->
                    let
                        keys =
                            case scope of
                                NonPlayer ->
                                    [ targetId |> KeyNotUnderList |> NonPlayerKey ]

                                CurrentPlayer ->
                                    [ targetId |> KeyNotUnderList |> PlayerKey turns.currentPlayerTurn ]

                                SpecificPlayer p ->
                                    [ targetId |> KeyNotUnderList |> PlayerKey p ]

                                AllPlayers ->
                                    playerIds turns |> List.map (\p -> targetId |> KeyNotUnderList |> PlayerKey p)

                                AllPlayersLists ->
                                    []

                                ThisPlayer ->
                                    case key of
                                        PlayerKey p _ ->
                                            [ targetId |> KeyNotUnderList |> PlayerKey p ]

                                        NonPlayerKey _ ->
                                            []
                    in
                    case idDefaultAndName targetId schema.tracker of
                        Just (default, name) ->
                            let
                                field k = { key=k, text=name }
                                newValue k currentValue =
                                    case ( op, currentValue ) of
                                        ( RestoreDefault, _ ) ->
                                            lookupDefault default k

                                        ( Adjust (WholeNumber amount), WholeNumber v ) ->
                                            v + amount |> WholeNumber

                                        ( Adjust (DecimalNumber amount), DecimalNumber v ) ->
                                            v + amount |> DecimalNumber

                                        ( Adjust (WholeNumber amount), DecimalNumber v ) ->
                                            v + toFloat amount |> DecimalNumber

                                        ( Adjust (DecimalNumber amount), WholeNumber v ) ->
                                            toFloat v + amount |> DecimalNumber

                                apply k res =
                                    res
                                        |> Result.andThen
                                            (\( st, tu, ls ) ->
                                                get k ( st, schema )
                                                    |> Result.mapError UnexpectedError
                                                    |> Result.map (\currentValue ->
                                                                       let
                                                                           new = newValue k currentValue
                                                                       in
                                                                           (set k new st, tu, ls ++ [ValueUpdated { field=field k, old=currentValue, new=new}])
                                                                  )
                                            )
                            in
                            List.foldl apply (Ok (state, turns, logs)) keys

                        Nothing ->
                            Err <| ComponentIdNotFound scope targetId


viewWithTile : Model -> Browser.Document Msg
viewWithTile model =
    { title =
        case model.state of
            DefinitionStage _ ->
                "b-b-r | Define Tracker"

            PlayerSelectionStage _ _ _ _ ->
                "b-b-r | Select Players"

            BigError _ _ ->
                "b-b-r | Error"

            TrackerStage schema _ _ _ _ ->
                "b-b-r | Track " ++ schema.name
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    case model.state of
        DefinitionStage valid ->
            viewEditTracker model.schemaJson model.url valid

        TrackerStage def state turns log aliases ->
            div []
                [ viewTracker def state turns aliases
                , viewLogs log state aliases |> Html.map (TrackerMsg def state turns aliases)
                , div [] [ makeUrl "track" model.schemaJson model.url ]
                , div [] [ makeUrl "edit" model.schemaJson model.url ]
                ]

        PlayerSelectionStage current players schema aliases ->
            viewPlayerSelection current players schema

        BigError prev err ->
            div []
                [ h1 [] [ text "Error" ]
                , case err of
                    ComponentIdNotFound NonPlayer id ->
                        text ("Could not find non-player component ID when applying effect: " ++ id)

                    ComponentIdNotFound _ id ->
                        text ("Could not find component ID in player-group when applying effect: " ++ id)

                    CouldNotReadNumberOfPlayers ->
                        text "Failed to read the number of players"

                    CouldNotParseWholeNumber raw ->
                        "Failed to read a numeric field as numeric, value: " ++ raw |> text

                    TooManyPlayerGroupsDefined ->
                        text "Too many player-groups are defined! Only one is allowed."

                    CannotSetCurrentPlayerToThisWithEffectsOutsideContext ->
                        text "To set this-player as the current player the effects must originate from the context of a player"

                    UnexpectedError message ->
                        "Unexpected error: " ++ message |> text
                , button [ onClick MoveToEdit ] [ text "Return to edit" ]
                , button [ onClick (MoveTo prev) ] [ text "Return to last state" ]
                ]


viewLogs : List Log -> TrackingState -> PlayerAliases -> Html TrackMsg
viewLogs entries state aliases =
    let
        disabled =
            case entries of
                [] -> True
                [GameStarted _] -> True
                _ -> False
    in
    div []
        [ h1 [] [ text "History" ]
        , button [ onClick UndoLastLog, Html.Attributes.disabled disabled, id "undo-last-history-entry" ] [ text "Undo" ]
        , List.map (viewLog aliases state) entries |> ul []
        ]


viewLog : PlayerAliases -> TrackingState -> Log -> Html a
viewLog aliases state log =
    li []
        [ case log of
            GameStarted turns ->
                if turns.playerCount > 1 && not turns.disabled then
                    text (String.fromInt turns.playerCount ++ " player game started, " ++ playerName turns.currentPlayerTurn aliases ++ "'s turn")

                else if not turns.disabled then
                    text "Game started"

                else
                    text "Start"

            ActionPerformed (PlayerKey player _) action [] ->
                text (playerName player aliases ++ " " ++ action)

            ActionPerformed (PlayerKey player _) action logs ->
                details []
                    [ summary [] [text (playerName player aliases ++ " " ++ action) ], logs |> List.map (viewLog aliases state) |> ul [] ]

            ActionPerformed (NonPlayerKey _) action [] ->
                text action

            ActionPerformed (NonPlayerKey _) action logs ->
                details []
                    [ summary [] [text action], logs |> List.map (viewLog aliases state) |> ul [] ]

            CurrentPlayerChanged {old,new} ->
                "Active player changed from " ++ playerName old aliases ++ " to " ++ playerName new aliases |> text

            ListItemAdded field ->
                let
                    itemSegment =
                      case getItemText field.key state of
                          Just t -> ", " ++ t
                          Nothing -> ""
                in
                  case field.key of
                      NonPlayerKey _ ->
                          text (field.text ++ itemSegment ++ " added")

                      PlayerKey player _ ->
                          text (playerName player aliases ++ itemSegment ++ " " ++ field.text ++ " added")

            ListItemRemoved field ->
                let
                    itemSegment =
                      case getItemText field.key state of
                          Just t -> ", " ++ t
                          Nothing -> ""
                in
                  case field.key of
                      NonPlayerKey _ ->
                          text (field.text ++ itemSegment ++ " removed")

                      PlayerKey player _ ->
                          text (playerName player aliases ++ itemSegment ++ " " ++ field.text ++ " removed")

            ValueUpdated s ->
                let
                    itemSegment =
                      case getItemText s.field.key state of
                          Just t -> ", " ++ t
                          Nothing -> ""
                in
                  case s.field.key of
                      NonPlayerKey _ ->
                          text (s.field.text ++ itemSegment ++ " updated from " ++ valueToString s.old ++ " to " ++ valueToString s.new)

                      PlayerKey player _ ->
                          text (playerName player aliases ++ itemSegment ++ "'s " ++ s.field.text ++ " updated from " ++ valueToString s.old ++ " to " ++ valueToString s.new)
          ]


viewPlayerSelection : Int -> { pgd | minPlayers : Int, maxPlayers : Int } -> TrackerTopLevelSchema -> Html Msg
viewPlayerSelection players { minPlayers, maxPlayers } schema =
    div []
        [ h1 [] [ text "Number of players" ]
        , input [ type_ "number", value (String.fromInt players), onInput (SetNumberOfPlayers schema), Html.Attributes.min (String.fromInt minPlayers), Html.Attributes.max (String.fromInt maxPlayers) ] []
        , button [ onClick (ConfirmNumberOfPlayers schema) ] [ text "Start game!" ]
        ]


makeUrl : String -> String -> Url -> Html Msg
makeUrl qp def url =
    a [ { url | query = qp ++ "=" ++ Url.percentEncode (Base64E.encode (Base64E.string def)) |> Just } |> Url.toString |> href, target "_blank", class (qp ++ "-link") ] [ "URL to " ++ qp |> text ]


viewEditTracker : String -> Url -> DefinitionValidity -> Html Msg
viewEditTracker def url valid =
    div []
        [ div [] [ h1 [] [ text "Edit Tracker" ] ]
        , div [] [ textarea [ cols 40, rows 10, placeholder "...", onInput UpdateDefinition, value def ] [] ]
        , case valid of
            CanDecodeWithoutErrors d ->
                div [class "definition-valid"]
                    [ div [] [ text <| "Definition good: " ++ d.name ]
                    , button [ onClick (MoveToPlayerSelection d), class "create-tracker-button" ] [ text "Create!" ]
                    ]

            CanDecodeWithErrors errs ->
                div [class "definition-invalid"]
                    [ text "Definition invalid"
                    , h2 [] [text "Errors"]
                    , errs
                      |> List.map (\e -> e |> Validation.errorToString |> text |> List.singleton |> li ["error-key-" ++ Validation.errorKey e|> id])
                      |> ul []
                    ]

            CannotDecode err ->
                div [class "definition-invalid"]
                    [ text "Definition invalid"
                    , pre [] [ text <| Decode.errorToString err ]
                    ]

            StartingOut ->
                text ""
        , div [] [ makeUrl "edit" def url ]
        ]


firstError : List (Result e v) -> Result e (List v)
firstError vs =
    case vs of
        [] ->
            Ok []

        res :: ress ->
            res |> Result.andThen (\v -> Result.map ((::) v) <| firstError ress)


eval : TrackerTopLevelSchema -> Turns -> Expression -> Key () -> TrackingState -> Int -> Result String Value
eval schema turns expr key state currentPlayer =
    let
        append oInt oFloat x y =
            case ( x, y ) of
                ( WholeNumber a, WholeNumber b ) ->
                    oInt a b |> WholeNumber

                ( WholeNumber a, DecimalNumber b ) ->
                    oFloat (toFloat a) b |> DecimalNumber

                ( DecimalNumber a, DecimalNumber b ) ->
                    oFloat a b |> DecimalNumber

                ( DecimalNumber a, WholeNumber b ) ->
                    oFloat a (toFloat b) |> DecimalNumber

        op k oInt oFloat a b =
            case b of
                Err err ->
                    Err err

                Ok v2 ->
                    case aux k a of
                        Ok v1 ->
                            append oInt oFloat v1 v2 |> Ok

                        Err err ->
                            Err err

        aux k e =
            case e of
                Op Add ops ->
                    List.foldl (op k (\a b -> a + b) (\a b -> a + b)) (WholeNumber 0 |> Ok) ops

                Op Mul ops ->
                    List.foldl (op  k(\a b -> a * b) (\a b -> a * b)) (WholeNumber 1 |> Ok) ops

                Op Sum [ Ref targetId AllPlayers ] ->
                    turns
                        |> playerIds
                        |> List.map (\p -> SpecificPlayer p |> Ref targetId |> aux (keyWithPlayerNumber p k))
                        |> firstError
                        |> Result.map (List.foldl (append (+) (+)) (WholeNumber 0))

                Op Sum [ Ref targetId AllPlayersLists ] ->
                    turns
                        |> playerIds
                        |> List.concatMap (\p -> Dict.get p state.player |> Maybe.map .listItems |> Maybe.withDefault Array.empty |> Array.toList |> List.map (\v -> ( p, v )))
                        |> List.filter (\( _, item ) -> item.live)
                        |> List.map (\( p, i ) -> get (k |> keyWithPlayerNumber p |> keyWithItemNumber i.index |> keyWithId targetId) ( state, schema ))
                        |> firstError
                        |> Result.map (List.foldl (append (+) (+)) (WholeNumber 0))

                Op Sum [ Ref targetId ThisPlayer ] ->
                    case k of
                        NonPlayerKey _ ->
                            "Found reference to this-player's " ++ targetId ++ "outside of player context!" |> Err

                        PlayerKey p _ ->
                            state.player
                                |> Dict.get p
                                |> Maybe.map .listItems
                                |> Maybe.withDefault Array.empty
                                |> Array.toList
                                |> List.filter (\i -> i.live)
                                |> List.map (\i -> get (k |> keyWithItemNumber i.index |> keyWithId targetId) ( state, schema ))
                                |> firstError
                                |> Result.map (List.foldl (append (+) (+)) (WholeNumber 0))

                Op Sum _ ->
                    Err "A sum must reference all players or a list"

                Literal v ->
                    Ok v

                Ref targetId scope ->
                    let
                        refKey =
                            case ( scope, k ) of
                                ( NonPlayer, _ ) ->
                                    targetId |> KeyNotUnderList |> NonPlayerKey |> Ok

                                ( CurrentPlayer, _ ) ->
                                    targetId |> KeyNotUnderList |> PlayerKey currentPlayer |> Ok

                                ( SpecificPlayer p, _ ) ->
                                    targetId |> KeyNotUnderList |> PlayerKey p |> Ok

                                ( AllPlayers, _ ) ->
                                    Err "Scalar calculated fields cannot reference all players"

                                ( AllPlayersLists, _ ) ->
                                    Err "Scalar calculated fields cannot reference lists"

                                ( ThisPlayer, PlayerKey this _ ) ->
                                    targetId |> KeyNotUnderList |> PlayerKey this |> Ok

                                ( ThisPlayer, NonPlayerKey _ ) ->
                                    "Referenced this player when finding " ++ targetId ++ " but couldn't find them" |> Err
                    in
                    case fieldsById targetId schema.tracker of
                        [ Calculated s ] ->
                            aux k s.equals

                        [ _ ] ->
                            refKey |> Result.andThen (\rk -> get rk ( state, schema ))

                        [] ->
                            "Tried to reference of field that does not exist, ID: " ++ targetId |> Err

                        _ ->
                            "IDs cannot be shared across multiple components, ID: " ++ targetId |> Err
    in
    aux key expr


viewTrackerComponent : TrackerTopLevelSchema -> TrackerSchema -> TrackingState -> Turns -> Key () -> PlayerAliases -> Html TrackMsg
viewTrackerComponent schema tracker state turns key aliases =
    case tracker of
        TextSchema s ->
            div [ key |> keyWithId s.text |> htmlIdFromKey "text" ]
                [ text s.text
                , text " "
                , case getItemText key state of
                    Just t -> input [ value t, onInput (SetItemText key) ] []
                    Nothing -> input [] []
                ]

        WholeNumberSchema s ->
            if s.hidden then
                div [] []

            else
                let
                    numberKey =
                        keyWithId s.id key

                    v =
                        valueToString <| Result.withDefault (lookupDefault s.default key) (get numberKey ( state, schema ))
                in
                div [ class "number-field", numberKey |> htmlIdFromKey "number"  ]
                    [ text s.text
                    , text " "
                    , if s.disabled then
                        span [class "number-value"] [ text v ]

                      else
                        let
                          limitAttr bound attr = bound |> Maybe.map (valueToString >> attr >> List.singleton) |> Maybe.withDefault []
                        in
                          input
                            (
                              [ type_ "number"
                              , onInput (SetWholeNumber { key = numberKey, text = s.text })
                              , value v
                              ]
                              ++ limitAttr s.min Html.Attributes.min
                              ++ limitAttr s.max Html.Attributes.max
                            )
                            []
                    ]

        Calculated s ->
            div [ class "calculated-field", class "number-field", key |> keyWithId (Maybe.withDefault s.text s.id) |> htmlIdFromKey "calculated" ]
                [ text s.text
                , text " "
                , case eval schema turns s.equals key state turns.currentPlayerTurn of
                    Ok (WholeNumber v) ->
                        [v |> String.fromInt |> text] |> span [class "calculation-result"]

                    Ok (DecimalNumber v) ->
                        [v |> Round.round 2 |> text] |> span [class "calculation-result"]

                    Err e ->
                        ["Error: " ++ e |> text] |> span [class "calculation-error"]
                , span [class "calculation-hint", s.equals |> expressionToString |> title] [text " 🛈"]
                ]

        ItemList s ->
            div
                [ style "border" "1px solid black"
                , class "item-list"
                ]
                [ div [] [ text s.text, button [ onClick (NewListItem { key=key, text=s.text }), htmlIdFromKey "add-item" (keyWithId s.id key) ] [ text "+" ] ]
                , listItems (keyWithId s.id key) state
                    |> Array.map
                        (\item ->
                            ( String.fromInt item.index
                            , if item.live then
                                let
                                    itemKey =
                                        keyWithItemNumber item.index key
                                in
                                div []
                                    [ viewTrackerComponent schema (Group { collapsed = Nothing, items = s.items, text = Nothing }) state turns itemKey aliases
                                    , button [ style "margin" "1rem", style "margin-top" "0", onClick (RemoveListItem { key=itemKey, text = s.text }), htmlIdFromKey "remove-item" (keyWithId s.id itemKey) ] [ text "Remove" ]
                                    ]

                              else
                                text ""
                            )
                        )
                    |> Array.toList
                    |> Keyed.node "div" []
                ]

        Group s ->
            let
                collapses = s.collapsed /= Nothing

                header = s.text |> Maybe.withDefault "" |> text

                group content =
                    div
                        [ style "border" "1px solid black", style "margin-left" "1rem", style "margin-right" "1rem", style "margin-bottom" "1rem" , style "margin-top" "1rem", class "group"]
                        [ case s.collapsed of
                            Just True ->
                              details [Html.Attributes.attribute "open" "true"] (summary [] [ header ] :: content)
                            Just False ->
                              details [] (summary [] [ header ] :: content)
                            Nothing ->
                              div [] content
                          ]
            in
            s.items |> List.map (\i -> viewTrackerComponent schema i state turns key aliases) |> group

        PlayerGroup s ->
            let
                common playerNumber =
                    [style "margin-top" "10px", class ("player-" ++ String.fromInt playerNumber ++ " " ++ if turns.currentPlayerTurn == playerNumber then "current-player" else "player")]

                currentPlayerIndicator playerNumber =
                    if turns.currentPlayerTurn == playerNumber && not turns.disabled then
                        [ style "border" "2px dashed gray", style "background-color" "lightyellow"] ++ common playerNumber

                    else
                        common playerNumber
            in
            playerIds turns
                |> List.map (\i -> div (currentPlayerIndicator i) [ viewPlayerIndicator turns i aliases, viewTrackerComponent schema (Group { items = s.items, collapsed = Nothing, text = Nothing }) state turns (keyWithPlayerNumber i key) aliases ])
                |> div []

        Action s ->
            button [ onClick (ApplyEffects key s.text s.effects), key |> keyWithId s.text |> htmlIdFromKey "action" ] [ text s.text ]


playerName : Int -> PlayerAliases -> String
playerName playerNumber aliases =
    Dict.get playerNumber aliases |> Maybe.withDefault ("Player " ++ String.fromInt (playerNumber + 1))


viewPlayerIndicator : Turns -> Int -> PlayerAliases -> Html TrackMsg
viewPlayerIndicator turns playerNumber aliases =
    div []
        [ input [ value (playerName playerNumber aliases), onInput (UpdatePlayerAlias playerNumber) ] []
        ]


viewTracker : TrackerTopLevelSchema -> TrackingState -> Turns -> PlayerAliases -> Html Msg
viewTracker schema state turns aliases =
    div []
        [ h1 [] [ text schema.name ]
        , Html.map (TrackerMsg schema state turns aliases) <| viewTrackerComponent schema schema.tracker state turns emptyKey aliases
        ]


lookupDefault : Defaults -> Key a -> Value
lookupDefault defaults key =
    case key of
        PlayerKey p _ ->
            defaults.playerDefaults |> Dict.get p |> Maybe.withDefault defaults.default

        NonPlayerKey _ ->
            defaults.default
