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
    | UnexpectedError String


type alias Field =
    { id : String, text : String }


type Log
    = GameStarted Turns
    | ActionPerformed (Maybe Int) String (List Effect)
    | ValueUpdated { player : Maybe Int, old : Value, new : Value, field : Field }


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
    = ApplyEffects (Maybe Int) String (List Effect)
    | SetWholeNumber Field (Maybe Int) String
    | UpdatePlayerAlias Int String


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
                                        if a.player == b.player && a.field == b.field then
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
                        log def Dict.empty turns (Just <| GameStarted turns) aliases

                    else
                        PlayerSelectionStage players.minPlayers players def aliases |> toState

                [] ->
                    let
                        turns =
                            { currentPlayerTurn = 0, playerCount = 1 }
                    in
                    log def Dict.empty turns (Just <| GameStarted turns) Dict.empty

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

        TrackerMsg schema state turns aliases (ApplyEffects thisPlayer action effects) ->
            case List.foldl (applyEffect thisPlayer) (Ok ( schema, state, turns )) effects of
                Ok ( sc, st, ts ) ->
                    log sc st ts (Just <| ActionPerformed thisPlayer action effects) aliases

                Err e ->
                    BigError e |> toState

        TrackerMsg schema state turns aliases (UpdatePlayerAlias playerNumber newAlias) ->
            log schema state turns Nothing (Dict.insert playerNumber newAlias aliases)

        TrackerMsg _ _ _ _ (SetWholeNumber _ _ "") ->
            model

        TrackerMsg schema state turns aliases (SetWholeNumber field player rawValue) ->
            case String.toInt rawValue of
                Just v ->
                    let
                        num =
                            WholeNumber v
                    in
                    case idDefault field.id schema.tracker of
                        Just default ->
                            let
                                oldValue =
                                    state |> Dict.get (key field.id player) |> Maybe.withDefault (lookupDefault default player)

                                event =
                                    ValueUpdated { player = player, old = oldValue, new = num, field = field }
                            in
                            if oldValue == num then
                                model

                            else
                                log schema (Dict.insert (key field.id player) num state) turns (Just <| event) aliases

                        Nothing ->
                            "Could not find old value for update" |> UnexpectedError |> BigError |> toState

                Nothing ->
                    CouldNotParseWholeNumber rawValue |> BigError |> toState

        ConfirmNumberOfPlayers schema ->
            case model.state of
                PlayerSelectionStage players bounds _ aliases ->
                    let
                        turns =
                            { currentPlayerTurn = 0, playerCount = players }
                    in
                    log schema Dict.empty turns (Just <| GameStarted turns) aliases

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
                                    [ ( Nothing, targetId ) ]

                                CurrentPlayer ->
                                    [ ( Just turns.currentPlayerTurn, key targetId (Just turns.currentPlayerTurn) ) ]

                                AllPlayers ->
                                    List.range 0 (turns.playerCount - 1) |> List.map (\i -> ( Just i, key targetId (Just i) ))

                                ThisPlayer ->
                                    List.range 0 (turns.playerCount - 1) |> List.map (\i -> ( Just i, key targetId (Just i) ))
                    in
                    case idDefault targetId schema.tracker of
                        Just default ->
                            let
                                newValue player currentValue =
                                    case ( op, currentValue ) of
                                        ( RestoreDefault, _ ) ->
                                            lookupDefault default player |> Ok

                                        ( Adjust (WholeNumber amount), WholeNumber v ) ->
                                            v + amount |> WholeNumber |> Ok

                                        ( Adjust (DecimalNumber amount), DecimalNumber v ) ->
                                            v + amount |> DecimalNumber |> Ok

                                        ( Adjust (WholeNumber amount), DecimalNumber v ) ->
                                            v + toFloat amount |> DecimalNumber |> Ok

                                        ( Adjust (DecimalNumber amount), WholeNumber v ) ->
                                            toFloat v + amount |> DecimalNumber |> Ok

                                apply ( player, scopedKey ) res =
                                    res
                                        |> Result.andThen
                                            (\( sc, st, tu ) ->
                                                newValue player (Maybe.withDefault (lookupDefault default player) (Dict.get scopedKey st)) |> Result.map (\v -> ( sc, Dict.insert scopedKey v st, tu ))
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

            ActionPerformed (Just player) action _ ->
                text (playerName player aliases ++ " " ++ action)

            ActionPerformed Nothing action _ ->
                text action

            ValueUpdated s ->
                case s.player of
                    Nothing ->
                        text (s.field.text ++ " updated from " ++ valueToString s.old ++ " to " ++ valueToString s.new)

                    Just player ->
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
        ]


eval : TrackerTopLevelSchema -> Expression -> Maybe Int -> TrackingState -> Int -> Result String Value
eval schema expr thisPlayer state currentPlayer =
    let
        op oInt oFloat a b =
            case b of
                Err err ->
                    Err err

                Ok (DecimalNumber v2) ->
                    case aux a of
                        Ok (WholeNumber v1) ->
                            Ok (DecimalNumber (oFloat (toFloat v1) v2))

                        Ok (DecimalNumber v1) ->
                            Ok (DecimalNumber (oFloat v1 v2))

                        Err err ->
                            Err err

                Ok (WholeNumber v2) ->
                    case aux a of
                        Ok (WholeNumber v1) ->
                            Ok (WholeNumber (oInt v1 v2))

                        Ok (DecimalNumber v1) ->
                            Ok (DecimalNumber (oFloat v1 (toFloat v2)))

                        Err err ->
                            Err err

        aux e =
            case e of
                Op Add ops ->
                    List.foldl (op (\a b -> a + b) (\a b -> a + b)) (WholeNumber 0 |> Ok) ops

                Op Mul ops ->
                    List.foldl (op (\a b -> a * b) (\a b -> a * b)) (WholeNumber 1 |> Ok) ops

                Literal v ->
                    Ok v

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
                            player |> Result.map (\p -> Maybe.withDefault (lookupDefault default p) (Dict.get (key targetId p) state))

                        Nothing ->
                            Err ("Could not find identifier " ++ targetId ++ " anywhere")
    in
    aux expr


viewTrackerComponent : TrackerTopLevelSchema -> TrackerSchema -> TrackingState -> Turns -> Maybe Int -> PlayerAliases -> Html TrackMsg
viewTrackerComponent schema tracker state turns playerNumber aliases =
    case tracker of
        WholeNumberSchema s ->
            if s.hidden then
                div [] []

            else
                let
                    v =
                        valueToString <| Maybe.withDefault (lookupDefault s.default playerNumber) (Dict.get (key s.id playerNumber) state)
                in
                div []
                    [ text s.text
                    , text " "
                    , if s.disabled then
                        text v

                      else
                        input [ type_ "number", onInput (SetWholeNumber { id = s.id, text = s.text } playerNumber), value v ] []
                    ]

        Calculated s ->
            div []
                [ text s.text
                , text " "
                , case eval schema s.equals playerNumber state turns.currentPlayerTurn of
                    Ok (WholeNumber v) ->
                        v |> String.fromInt |> text

                    Ok (DecimalNumber v) ->
                        v |> Round.round 2 |> text

                    Err e ->
                        text ("Error: " ++ e)
                ]

        Group s ->
            div [] (List.map (\i -> viewTrackerComponent schema i state turns playerNumber aliases) s.items)

        PlayerGroup s ->
            List.range 0 (turns.playerCount - 1) |> List.map (\i -> div [] [ h2 [] [ viewPlayerIndicator turns i aliases ], viewTrackerComponent schema (Group { items = s.items }) state turns (Just i) aliases ]) |> div []

        Action s ->
            button [ onClick (ApplyEffects playerNumber s.text s.effects) ] [ text s.text ]


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
        , Html.map (TrackerMsg schema state turns aliases) <| viewTrackerComponent schema schema.tracker state turns Nothing aliases
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
    | ThisPlayer


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
    Decode.map (\items -> Group { items = items }) <| field "items" (Decode.list trackerSchemaDecoder)


actionDecoder : Decoder TrackerSchema
actionDecoder =
    Decode.map2
        (\text effects -> Action { text = text, effects = effects })
        (field "text" string)
        (field "effects" (Decode.list effectDecoder))


opDecoder : Operator -> Decoder Expression
opDecoder op =
    Decode.map (\ops -> Op op ops) (field "ops" (Decode.list expressionDecoder))


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


type Operator
    = Add
    | Mul


type Expression
    = Op Operator (List Expression)
    | Ref String CellScope
    | Literal Value


lookupDefault : Defaults -> Maybe Int -> Value
lookupDefault defaults player =
    case player of
        Just p ->
            defaults.playerDefaults |> Dict.get p |> Maybe.withDefault defaults.default

        Nothing ->
            defaults.default


type alias Defaults =
    { playerDefaults : Dict Int Value, default : Value }


type TrackerSchema
    = PlayerGroup { items : List TrackerSchema, minPlayers : Int, maxPlayers : Int, defaultAliases : List String }
    | Group { items : List TrackerSchema }
    | Action { text : String, effects : List Effect }
    | WholeNumberSchema { text : String, default : Defaults, id : String, disabled : Bool, hidden : Bool }
    | Calculated { text : String, equals : Expression }


newPlayerAliases : List String -> PlayerAliases
newPlayerAliases vs =
    List.indexedMap Tuple.pair vs |> Dict.fromList


findPlayerGroup : TrackerSchema -> List ( { minPlayers : Int, maxPlayers : Int }, PlayerAliases )
findPlayerGroup schema =
    case schema of
        WholeNumberSchema s ->
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
