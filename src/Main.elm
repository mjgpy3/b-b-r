module Main exposing (..)

import Base64.Decode as Base64D
import Base64.Encode as Base64E
import Browser
import Debug as Debug
import Dict exposing (Dict)
import Html exposing (Html, a, b, button, details, div, h1, h2, hr, i, input, pre, summary, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (Decoder, field, int, map, map4, string)
import Json.Encode as E
import Maybe exposing (Maybe)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import Round as Round
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
            ( init url |> update (UpdateDefinition schema), Cmd.none )

        ( Nothing, Just schema ) ->
            case Decode.decodeString trackerTopLevelSchemaDecoder schema of
                Err e ->
                    ( { schemaJson = schema, url = url, state = InvalidDefinition e |> DefinitionStage }, Cmd.none )

                Ok d ->
                    ( { schemaJson = schema, url = url, state = (init url).state } |> update (MoveToPlayerSelection d), Cmd.none )

        _ ->
            ( init url, Cmd.none )


type DefinitionValidity
    = ValidDefinition TrackerTopLevelSchema
    | InvalidDefinition Decode.Error
    | StartingOut


type Value
    = WholeNumber Int
    | DecimalNumber Float


valueToString : Value -> String
valueToString v =
    case v of
        WholeNumber n ->
            String.fromInt n

        DecimalNumber n ->
            String.fromFloat n


type KeyMaybeUnderList v
    = KeyUnderList Int v
    | KeyNotUnderList v


type Key v
    = NonPlayerKey (KeyMaybeUnderList v)
    | PlayerKey Int (KeyMaybeUnderList v)


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


type alias MaybeUnderList =
    { notUnderList : Dict String Value
    , underList : Dict Int (Dict String Value)
    , listItems : List { index : Int, live : Bool }
    }


listItems : Key String -> TrackingState -> List { index : Int, live : Bool }
listItems key state =
    case key of
        NonPlayerKey _ ->
            state.nonPlayer.listItems

        PlayerKey p _ ->
            state.player
                |> Dict.get p
                |> Maybe.map .listItems
                |> Maybe.withDefault []


type alias TrackingState =
    { nonPlayer : MaybeUnderList
    , player : Dict Int MaybeUnderList
    }


emptyMul =
    { notUnderList = Dict.empty
    , underList = Dict.empty
    , listItems = []
    }


emptyState : TrackingState
emptyState =
    { nonPlayer =
        { notUnderList = Dict.empty
        , underList = Dict.empty
        , listItems = []
        }
    , player = Dict.empty
    }


removeItem : Key k -> TrackingState -> TrackingState
removeItem key state =
    let
        removeListItem idx mul =
            case mul of
                Just m ->
                    { m | listItems = List.map (\item -> { item | live = item.live && item.index /= idx }) m.listItems }

                Nothing ->
                    emptyMul
    in
    case key of
        NonPlayerKey (KeyUnderList idx _) ->
            { state | nonPlayer = removeListItem idx (Just state.nonPlayer) }

        PlayerKey p (KeyUnderList idx _) ->
            { state | player = Dict.update p (removeListItem idx >> Just) state.player }

        NonPlayerKey (KeyNotUnderList _) ->
            state

        PlayerKey _ (KeyNotUnderList _) ->
            state


addItem : Key k -> TrackingState -> TrackingState
addItem key state =
    let
        addTo items =
            { live = True, index = List.length items } :: items

        addListItem mul =
            case mul of
                Just m ->
                    { m | listItems = addTo m.listItems }

                Nothing ->
                    { emptyMul | listItems = addTo [] }
    in
    case key of
        NonPlayerKey _ ->
            { state | nonPlayer = addListItem (Just state.nonPlayer) }

        PlayerKey p _ ->
            { state | player = Dict.update p (addListItem >> Just) state.player }


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
                    { listItems = [], underList = Dict.empty, notUnderList = Dict.singleton id value }

                ( KeyNotUnderList id, Just s ) ->
                    { s | notUnderList = Dict.insert id value s.notUnderList }

                ( KeyUnderList idx id, Nothing ) ->
                    { listItems = [], notUnderList = Dict.empty, underList = Dict.singleton idx (Dict.singleton id value) }

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
    }


type Error
    = ComponentIdNotFound CellScope String
    | CouldNotReadNumberOfPlayers
    | CouldNotParseWholeNumber String
    | TooManyPlayerGroupsDefined
    | CannotSetCurrentPlayerToThisWithEffectsOutsideContext
    | UnexpectedError String


type alias Field =
    { id : String, text : String }


type Log
    = GameStarted Turns
    | ActionPerformed (Key ()) String (List Effect)
    | ValueUpdated { key : Key String, old : Value, new : Value, field : Field }


type alias PlayerAliases =
    Dict Int String


type StageState
    = DefinitionStage DefinitionValidity
    | PlayerSelectionStage Int { minPlayers : Int, maxPlayers : Int } TrackerTopLevelSchema PlayerAliases
    | BigError Error
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


type TrackMsg
    = ApplyEffects (Key ()) String (List Effect)
    | SetWholeNumber Field (Key String) String
    | UpdatePlayerAlias Int String
    | NewListItem (Key ()) Field
    | RemoveListItem (Key ())


type Msg
    = UpdateDefinition String
    | MoveToPlayerSelection TrackerTopLevelSchema
    | MoveToEdit
    | TrackerMsg TrackerTopLevelSchema TrackingState Turns PlayerAliases TrackMsg
    | SetNumberOfPlayers TrackerTopLevelSchema String
    | ConfirmNumberOfPlayers TrackerTopLevelSchema
    | Noop


update : Msg -> Model -> Model
update msg model =
    let
        toState state =
            { schemaJson = model.schemaJson, url = model.url, state = state }

        log schema state players newLog aliases =
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
                                        if a.key == b.key && a.field == b.field then
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
            TrackerStage schema state players compacted aliases |> toState
    in
    case msg of
        Noop ->
            model

        MoveToPlayerSelection def ->
            case findPlayerGroup def.tracker of
                [ ( players, aliases ) ] ->
                    if players.minPlayers == players.maxPlayers then
                        let
                            turns =
                                { currentPlayerTurn = 0, playerCount = players.maxPlayers }
                        in
                        log def emptyState turns (Just <| GameStarted turns) aliases

                    else
                        PlayerSelectionStage players.minPlayers players def aliases |> toState

                [] ->
                    let
                        turns =
                            { currentPlayerTurn = 0, playerCount = 1 }
                    in
                    log def emptyState turns (Just <| GameStarted turns) Dict.empty

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

        TrackerMsg schema state turns aliases (ApplyEffects key action effects) ->
            case List.foldl (applyEffect key) (Ok ( schema, state, turns )) effects of
                Ok ( sc, st, ts ) ->
                    log sc st ts (Just <| ActionPerformed key action effects) aliases

                Err e ->
                    BigError e |> toState

        TrackerMsg schema state turns aliases (UpdatePlayerAlias playerNumber newAlias) ->
            log schema state turns Nothing (Dict.insert playerNumber newAlias aliases)

        TrackerMsg _ _ _ _ (SetWholeNumber _ _ "") ->
            model

        TrackerMsg schema state turns aliases (NewListItem key field) ->
            log schema (addItem key state) turns Nothing aliases

        TrackerMsg schema state turns aliases (RemoveListItem key) ->
            log schema (removeItem key state) turns Nothing aliases

        TrackerMsg schema state turns aliases (SetWholeNumber field key rawValue) ->
            case String.toInt rawValue of
                Just v ->
                    let
                        num =
                            WholeNumber v
                    in
                    case get key ( state, schema ) of
                        Ok oldValue ->
                            let
                                event =
                                    ValueUpdated { key = key, old = oldValue, new = num, field = field }
                            in
                            if oldValue == num then
                                model

                            else
                                log schema (set key num state) turns (Just <| event) aliases

                        Err e ->
                            e |> UnexpectedError |> BigError |> toState

                Nothing ->
                    CouldNotParseWholeNumber rawValue |> BigError |> toState

        ConfirmNumberOfPlayers schema ->
            case model.state of
                PlayerSelectionStage players bounds _ aliases ->
                    let
                        turns =
                            { currentPlayerTurn = 0, playerCount = players }
                    in
                    log schema emptyState turns (Just <| GameStarted turns) aliases

                _ ->
                    BigError CouldNotReadNumberOfPlayers |> toState

        SetNumberOfPlayers _ "" ->
            model

        SetNumberOfPlayers schema n ->
            case ( String.toInt n, model.state ) of
                ( Just players, PlayerSelectionStage _ bounds _ aliases ) ->
                    PlayerSelectionStage players bounds schema aliases |> toState

                --                    TrackerStage schema Dict.empty { currentPlayerTurn = 0, playerCount = players } |> toState
                _ ->
                    BigError CouldNotReadNumberOfPlayers |> toState


applyEffect : Key a -> Effect -> Result Error ( TrackerTopLevelSchema, TrackingState, Turns ) -> Result Error ( TrackerTopLevelSchema, TrackingState, Turns )
applyEffect key eff result =
    case result of
        Err e ->
            Err e

        Ok ( schema, state, turns ) ->
            case eff of
                NextTurn ->
                    Ok ( schema, state, { turns | currentPlayerTurn = modBy turns.playerCount (turns.currentPlayerTurn + 1) } )

                SetCurrentPlayer CurrentIsThisPlayer ->
                    case key of
                        PlayerKey player _ ->
                            Ok ( schema, state, { turns | currentPlayerTurn = player } )

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
                    case idDefault targetId schema.tracker of
                        Just default ->
                            let
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
                                            (\( sc, st, tu ) ->
                                                get k ( st, schema )
                                                    |> Result.mapError UnexpectedError
                                                    |> Result.map (\currentValue -> ( sc, set k (newValue k currentValue) st, tu ))
                                            )
                            in
                            List.foldl apply (Ok ( schema, state, turns )) keys

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

            BigError _ ->
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
                , viewLogs log aliases
                , div [] [ makeUrl "track" model.schemaJson model.url ]
                , div [] [ makeUrl "edit" model.schemaJson model.url ]
                ]

        PlayerSelectionStage current players schema aliases ->
            viewPlayerSelection current players schema

        BigError err ->
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
                ]


viewLogs : List Log -> PlayerAliases -> Html Msg
viewLogs entries aliases =
    div [] (h1 [] [ text "History" ] :: List.map (viewLog aliases) entries)


viewLog : PlayerAliases -> Log -> Html Msg
viewLog aliases log =
    div []
        [ case log of
            GameStarted turns ->
                if turns.playerCount > 1 then
                    text (String.fromInt turns.playerCount ++ " player game started, " ++ playerName turns.currentPlayerTurn aliases ++ "'s turn")

                else
                    text "Game started"

            ActionPerformed (PlayerKey player _) action _ ->
                text (playerName player aliases ++ " " ++ action)

            ActionPerformed (NonPlayerKey _) action _ ->
                text action

            ValueUpdated s ->
                case s.key of
                    NonPlayerKey _ ->
                        text (s.field.text ++ " updated from " ++ valueToString s.old ++ " to " ++ valueToString s.new)

                    PlayerKey player _ ->
                        text (playerName player aliases ++ "'s " ++ s.field.text ++ " updated from " ++ valueToString s.old ++ " to " ++ valueToString s.new)
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
    a [ { url | query = qp ++ "=" ++ Url.percentEncode (Base64E.encode (Base64E.string def)) |> Just } |> Url.toString |> href, target "_blank" ] [ "URL to " ++ qp |> text ]


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

        op oInt oFloat a b =
            case b of
                Err err ->
                    Err err

                Ok v2 ->
                    case aux a of
                        Ok v1 ->
                            append oInt oFloat v1 v2 |> Ok

                        Err err ->
                            Err err

        aux e =
            case e of
                Op Add ops ->
                    List.foldl (op (\a b -> a + b) (\a b -> a + b)) (WholeNumber 0 |> Ok) ops

                Op Mul ops ->
                    List.foldl (op (\a b -> a * b) (\a b -> a * b)) (WholeNumber 1 |> Ok) ops

                Op Sum [ Ref targetId AllPlayers ] ->
                    turns
                        |> playerIds
                        |> List.map (SpecificPlayer >> Ref targetId >> aux)
                        |> firstError
                        |> Result.map (List.foldl (append (+) (+)) (WholeNumber 0))

                Op Sum [ Ref targetId AllPlayersLists ] ->
                    turns
                        |> playerIds
                        |> List.concatMap (\p -> Dict.get p state.player |> Maybe.map .listItems |> Maybe.withDefault [] |> List.map (\v -> (p, v)))
                        |> List.filter (\(_, item) -> item.live)
                        |> List.map (\(p, i) -> get (key |> keyWithPlayerNumber p |> keyWithItemNumber i.index |> keyWithId targetId) ( state, schema ))
                        |> firstError
                        |> Result.map (List.foldl (append (+) (+)) (WholeNumber 0))

                Op Sum [ Ref targetId ThisPlayer ] ->
                    case key of
                        NonPlayerKey _ ->
                            "Found reference to this-player's " ++ targetId ++ "outside of player context!" |> Err

                        PlayerKey p _ ->
                            state.player
                                |> Dict.get p
                                |> Maybe.map .listItems
                                |> Maybe.withDefault []
                                |> List.filter (\i -> i.live)
                                |> List.map (\i -> get (key |> keyWithItemNumber i.index |> keyWithId targetId) ( state, schema ))
                                |> firstError
                                |> Result.map (List.foldl (append (+) (+)) (WholeNumber 0))

                Op Sum _ ->
                    Err "A sum must reference all players or a list"

                Literal v ->
                    Ok v

                Ref targetId scope ->
                    let
                        refKey =
                            case ( scope, key ) of
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
                                    Err "Referenced this player, but couldn't find them"
                    in
                        case fieldsById targetId schema.tracker of
                            [Calculated s] -> aux s.equals
                            [_] -> refKey |> Result.andThen (\k -> get k ( state, schema ))
                            [] -> "Tried to reference of field that does not exist, ID: " ++ targetId |> Err
                            _ -> "IDs cannot be shared across multiple components, ID: " ++ targetId |> Err
    in
    aux expr


viewTrackerComponent : TrackerTopLevelSchema -> TrackerSchema -> TrackingState -> Turns -> Key () -> PlayerAliases -> Html TrackMsg
viewTrackerComponent schema tracker state turns key aliases =
    case tracker of
        TextSchema s ->
            div []
                [ text s.text
                , text " "
                , input [] []
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
                div []
                    [ text s.text
                    , text " "
                    , if s.disabled then
                        text v

                      else
                        input [ type_ "number", onInput (SetWholeNumber { id = s.id, text = s.text } numberKey), value v ] []
                    ]

        Calculated s ->
            div []
                [ text s.text
                , text " "
                , case eval schema turns s.equals key state turns.currentPlayerTurn of
                    Ok (WholeNumber v) ->
                        v |> String.fromInt |> text

                    Ok (DecimalNumber v) ->
                        v |> Round.round 2 |> text

                    Err e ->
                        text ("Error: " ++ e)
                ]

        ItemList s ->
            div
                [ style "border" "1px solid black"
                ]
                [ div [] [ text s.text, button [ onClick (NewListItem key { id = s.id, text = s.text }) ] [ text "+" ] ]
                , listItems (keyWithId s.id key) state
                    |> List.map
                        (\item ->
                            ( String.fromInt item.index
                            , if item.live then
                                let
                                    itemKey =
                                        keyWithItemNumber item.index key
                                in
                                div []
                                    [ viewTrackerComponent schema (Group { collapsed = Just False, items = s.items, text = Nothing }) state turns itemKey aliases
                                    , button [ style "margin" "1rem", style "margin-top" "0", onClick (RemoveListItem itemKey) ] [ text "Remove" ]
                                    ]

                              else
                                text ""
                            )
                        )
                    |> Keyed.node "div" []
                ]

        Group s ->
            let
                collapses =
                    s.collapsed == Just True

                header =
                    case s.text of
                        Just t ->
                            div [] [ b [] [ text t ] ]

                        Nothing ->
                            div [] []

                group content =
                    div
                        [ style "border" "1px solid black", style "margin-left" "1rem", style "margin-right" "1rem" ]
                        [ if collapses then
                            details [] (summary [] [ header ] :: content)

                          else
                            div [] content
                        ]
            in
            s.items |> List.map (\i -> viewTrackerComponent schema i state turns key aliases) |> group

        PlayerGroup s ->
            playerIds turns |> List.map (\i -> div [] [ h2 [] [ viewPlayerIndicator turns i aliases ], viewTrackerComponent schema (Group { items = s.items, collapsed = Nothing, text = Nothing }) state turns (keyWithPlayerNumber i key) aliases ]) |> div []

        Action s ->
            button [ onClick (ApplyEffects key s.text s.effects) ] [ text s.text ]


playerName : Int -> PlayerAliases -> String
playerName playerNumber aliases =
    Dict.get playerNumber aliases |> Maybe.withDefault ("Player " ++ String.fromInt (playerNumber + 1))


viewPlayerIndicator : Turns -> Int -> PlayerAliases -> Html TrackMsg
viewPlayerIndicator turns playerNumber aliases =
    let
        currentPlayerIndicator =
            if turns.currentPlayerTurn == playerNumber then
                style "border" "4px solid black"

            else
                style "border-left" "0px"
    in
    div [ currentPlayerIndicator ]
        [ input [ value (playerName playerNumber aliases), onInput (UpdatePlayerAlias playerNumber) ] []
        ]


viewTracker : TrackerTopLevelSchema -> TrackingState -> Turns -> PlayerAliases -> Html Msg
viewTracker schema state turns aliases =
    div []
        [ h1 [] [ text schema.name ]
        , Html.map (TrackerMsg schema state turns aliases) <| viewTrackerComponent schema schema.tracker state turns emptyKey aliases
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

        Just "all-players-lists" ->
            Decode.succeed AllPlayersLists

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
            Decode.map3 (\a -> OnCells (Adjust a)) (field "amount" numberDecoder) (field "targetId" string) (Decode.maybe (field "scope" string) |> Decode.andThen cellScopeDecoder)

        _ ->
            Decode.fail (ty ++ " is not a valid effect type")


effectDecoder : Decoder Effect
effectDecoder =
    field "type" string |> Decode.andThen specificEffectDecoder


type CellEffect
    = RestoreDefault
    | Adjust Value


type CellScope
    = NonPlayer
    | CurrentPlayer
    | AllPlayers
    | AllPlayersLists
    | ThisPlayer
    | SpecificPlayer Int


type NewCurrentPlayer
    = CurrentIsThisPlayer


type Effect
    = OnCells CellEffect String CellScope
    | NextTurn
    | SetCurrentPlayer NewCurrentPlayer


playerGroupDecoder : Decoder TrackerSchema
playerGroupDecoder =
    Decode.map4
        (\items minPlayers maxPlayers defaultAliases -> PlayerGroup { items = items, minPlayers = minPlayers, maxPlayers = maxPlayers, defaultAliases = Maybe.withDefault [] defaultAliases })
        (field "items" (Decode.list trackerSchemaDecoder))
        (field "minPlayers" Decode.int)
        (field "maxPlayers" Decode.int)
        (Decode.maybe (field "defaultAliases" (Decode.list Decode.string)))


groupDecoder : Decoder TrackerSchema
groupDecoder =
    Decode.map3 (\items collapsed text -> Group { items = items, collapsed = collapsed, text = text })
        (field "items" (Decode.list trackerSchemaDecoder))
        (Decode.maybe (field "collapsed" Decode.bool))
        (Decode.maybe (field "text" Decode.string))


actionDecoder : Decoder TrackerSchema
actionDecoder =
    Decode.map2
        (\text effects -> Action { text = text, effects = effects })
        (field "text" string)
        (field "effects" (Decode.list effectDecoder))


opDecoder : Operator -> Decoder Expression
opDecoder op =
    Decode.map (\ops -> Op op ops) (field "ops" (Decode.list expressionDecoder))


sumDecoder : Decoder Expression
sumDecoder =
    Decode.map (\op -> Op Sum [ op ]) (field "of" expressionDecoder)


refDecoder : Decoder Expression
refDecoder =
    Decode.map2 Ref (field "targetId" string) (Decode.maybe (field "scope" string) |> Decode.andThen cellScopeDecoder)


numberDecoder : Decoder Value
numberDecoder =
    Decode.oneOf [ Decode.map WholeNumber Decode.int, Decode.map DecimalNumber Decode.float ]


literalDecoder : Decoder Expression
literalDecoder =
    Decode.map Literal (field "value" numberDecoder)


specificExpressionDecoder : String -> Decoder Expression
specificExpressionDecoder ty =
    case ty of
        "add" ->
            opDecoder Add

        "mul" ->
            opDecoder Mul

        "sum" ->
            sumDecoder

        "ref" ->
            refDecoder

        "literal" ->
            literalDecoder

        _ ->
            Decode.fail (ty ++ " is not a valid expression type")


expressionDecoder : Decoder Expression
expressionDecoder =
    field "type" string |> Decode.andThen specificExpressionDecoder


wholeNumberDecoder : Decoder TrackerSchema
wholeNumberDecoder =
    let
        playerDefaultsDecoder =
            Decode.map Dict.fromList
                (field "playerDefaults"
                    (Decode.list (Decode.map2 (\a b -> ( a, b )) (field "player" Decode.int) (field "default" numberDecoder)))
                )
    in
    Decode.map6
        (\text default id disabled hidden playerDefaults -> WholeNumberSchema { text = text, default = { playerDefaults = playerDefaults, default = default }, id = id, disabled = disabled == Just True, hidden = hidden == Just True })
        (field "text" string)
        (field "default" numberDecoder)
        (field "id" string)
        (Decode.maybe (field "disabled" Decode.bool))
        (Decode.maybe (field "hidden" Decode.bool))
        (Decode.map (Maybe.withDefault Dict.empty) <| Decode.maybe playerDefaultsDecoder)


textDecoder : Decoder TrackerSchema
textDecoder =
    Decode.map2
        (\text id -> TextSchema { text = text, id = id })
        (field "text" string)
        (field "id" string)


calculatedDecoder : Decoder TrackerSchema
calculatedDecoder =
    Decode.map3
        (\text equals id -> Calculated { text = text, equals = equals, id = id })
        (field "text" string)
        (field "equals" expressionDecoder)
        (Decode.maybe (field "id" Decode.string))


itemListDecoder : Decoder TrackerSchema
itemListDecoder =
    Decode.map3
        (\text id items -> ItemList { text = text, id = id, items = items })
        (field "text" string)
        (field "id" string)
        (field "items" (Decode.list trackerSchemaDecoder))


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

        "text" ->
            textDecoder

        "calculated" ->
            calculatedDecoder

        "item-list" ->
            itemListDecoder

        _ ->
            Decode.fail (ty ++ " is not a valid tracker component type")


trackerSchemaDecoder : Decoder TrackerSchema
trackerSchemaDecoder =
    field "type" string |> Decode.andThen specificTrackerSchemaDecoder


type Operator
    = Add
    | Mul
    | Sum


type Expression
    = Op Operator (List Expression)
    | Ref String CellScope
    | Literal Value


lookupDefault : Defaults -> Key a -> Value
lookupDefault defaults key =
    case key of
        PlayerKey p _ ->
            defaults.playerDefaults |> Dict.get p |> Maybe.withDefault defaults.default

        NonPlayerKey _ ->
            defaults.default


type alias Defaults =
    { playerDefaults : Dict Int Value, default : Value }


type TrackerSchema
    = PlayerGroup { items : List TrackerSchema, minPlayers : Int, maxPlayers : Int, defaultAliases : List String }
    | Group { items : List TrackerSchema, collapsed : Maybe Bool, text : Maybe String }
    | Action { text : String, effects : List Effect }
    | WholeNumberSchema { text : String, default : Defaults, id : String, disabled : Bool, hidden : Bool }
    | TextSchema { text : String, id : String }
    | Calculated { text : String, equals : Expression, id : Maybe String }
    | ItemList { text : String, id : String, items : List TrackerSchema }


newPlayerAliases : List String -> PlayerAliases
newPlayerAliases vs =
    List.indexedMap Tuple.pair vs |> Dict.fromList


findPlayerGroup : TrackerSchema -> List ( { minPlayers : Int, maxPlayers : Int }, PlayerAliases )
findPlayerGroup schema =
    case schema of
        WholeNumberSchema s ->
            []

        TextSchema s ->
            []

        -- Allowing users to put a player group in here is a bad idea
        ItemList s ->
            []

        Group s ->
            List.concatMap findPlayerGroup s.items

        PlayerGroup s ->
            ( { minPlayers = s.minPlayers, maxPlayers = s.maxPlayers }, newPlayerAliases s.defaultAliases ) :: List.concatMap findPlayerGroup s.items

        Action _ ->
            []

        Calculated _ ->
            []


idDefault : String -> TrackerSchema -> Maybe Defaults
idDefault id schema =
    case schema of
        TextSchema s ->
            Nothing

        WholeNumberSchema s ->
            if s.id == id then
                Just s.default

            else
                Nothing

        Group s ->
            List.head <| List.filterMap (idDefault id) s.items

        ItemList s ->
            List.head <| List.filterMap (idDefault id) s.items

        PlayerGroup s ->
            List.head <| List.filterMap (idDefault id) s.items

        Action s ->
            Nothing

        Calculated _ ->
            Nothing


fieldsById : String -> TrackerSchema -> (List TrackerSchema)
fieldsById id schema =
    case schema of
        TextSchema s ->
            []

        WholeNumberSchema s ->
            if s.id == id then
                [WholeNumberSchema s]

            else
                []

        Group s ->
            List.concatMap (fieldsById id) s.items

        ItemList s ->
            if s.id == id
            then ItemList s::List.concatMap (fieldsById id) s.items
            else List.concatMap (fieldsById id) s.items

        PlayerGroup s ->
            List.concatMap (fieldsById id) s.items

        Action s ->
            []

        Calculated s ->
            if s.id == Just id then
                [Calculated s]

            else
                []

trackerTopLevelSchemaDecoder : Decoder TrackerTopLevelSchema
trackerTopLevelSchemaDecoder =
    Decode.map2 TrackerTopLevelSchema (field "name" string) (field "tracker" trackerSchemaDecoder)


type alias TrackerTopLevelSchema =
    { name : String
    , tracker : TrackerSchema
    }
