module TrackerSchema exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, field, int, map, map4, string)
import Maybe exposing (Maybe)
import String exposing (String)
import Set exposing (Set)

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

scopeToString : CellScope -> String
scopeToString scope =
    case scope of
      NonPlayer -> ""
      CurrentPlayer -> "[active]"
      AllPlayers -> "[all]"
      AllPlayersLists -> "all-items"
      ThisPlayer -> "[this]"
      SpecificPlayer idx -> "[" ++ String.fromInt idx ++ "]"

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
    Decode.map8
        (\text default id disabled hidden playerDefaults min max -> WholeNumberSchema { text = text, default = { playerDefaults = playerDefaults, default = default }, id = id, disabled = disabled == Just True, hidden = hidden == Just True, min=min, max=max })
        (field "text" string)
        (field "default" numberDecoder)
        (field "id" string)
        (Decode.maybe (field "disabled" Decode.bool))
        (Decode.maybe (field "hidden" Decode.bool))
        (Decode.map (Maybe.withDefault Dict.empty) <| Decode.maybe playerDefaultsDecoder)
        (Decode.maybe (field "min" numberDecoder))
        (Decode.maybe (field "max" numberDecoder))


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

operatorToString : Operator -> String
operatorToString op =
    case op of
        Add -> "+"
        Mul -> "*"
        Sum -> "+"

type Expression
    = Op Operator (List Expression)
    | Ref String CellScope
    | Literal Value

expressionToString : Expression -> String
expressionToString expr =
    case expr of
      Op Sum [e] -> "sum(" ++ expressionToString e ++ ")"
      Op op exprs -> exprs |> List.map expressionToString |> String.join (" " ++ operatorToString op ++ " ") |> \v -> "(" ++ v ++ ")"
      Ref id scope -> id ++ scopeToString scope
      Literal v -> valueToString v

type Value
    = WholeNumber Int
    | DecimalNumber Float

valueToFloat : Value -> Float
valueToFloat v =
    case v of
      WholeNumber n -> toFloat n
      DecimalNumber f -> f

valueToString : Value -> String
valueToString v =
    case v of
        WholeNumber n ->
            String.fromInt n

        DecimalNumber n ->
            String.fromFloat n

leq : Value -> Value -> Bool
leq x y =
  case (x, y) of
      (WholeNumber a, WholeNumber b) -> a <= b
      (a, b) -> valueToFloat a <= valueToFloat b

type alias Defaults =
    { playerDefaults : Dict Int Value, default : Value }

type alias PlayerGroupData = { items : List TrackerSchema, minPlayers : Int, maxPlayers : Int, defaultAliases : List String }

type TrackerSchema
    = PlayerGroup PlayerGroupData
    | Group { items : List TrackerSchema, collapsed : Maybe Bool, text : Maybe String }
    | Action { text : String, effects : List Effect }
    | WholeNumberSchema { text : String, default : Defaults, id : String, disabled : Bool, hidden : Bool, min : Maybe Value, max : Maybe Value }
    | TextSchema { text : String, id : String }
    | Calculated { text : String, equals : Expression, id : Maybe String }
    | ItemList { text : String, id : String, items : List TrackerSchema }

type alias PlayerAliases =
    Dict Int String

newPlayerAliases : List String -> PlayerAliases
newPlayerAliases vs =
    List.indexedMap Tuple.pair vs |> Dict.fromList


findPlayerGroup : TrackerSchema -> List ( PlayerGroupData, PlayerAliases )
findPlayerGroup schema =
    case schema of
        WholeNumberSchema s ->
            []

        TextSchema s ->
            []

        ItemList s ->
            []

        Group s ->
            List.concatMap findPlayerGroup s.items

        PlayerGroup s ->
            ( s, newPlayerAliases s.defaultAliases ) :: List.concatMap findPlayerGroup s.items

        Action _ ->
            []

        Calculated _ ->
            []

numericFieldIds : TrackerSchema -> List (String, TrackerSchema)
numericFieldIds schema =
    case schema of
        TextSchema s ->
            []

        WholeNumberSchema s ->
            [(s.id, WholeNumberSchema s)]

        Group s ->
            List.concatMap numericFieldIds s.items

        ItemList s ->
            List.concatMap numericFieldIds s.items

        PlayerGroup s ->
            List.concatMap numericFieldIds s.items

        Action s ->
            []

        Calculated s ->
            case s.id of
              Just id -> [(id, Calculated s)]
              Nothing -> []

targetIds : TrackerSchema -> List String
targetIds schema =
    let
        effectTargetIds eff =
            case eff of
              OnCells _ targetId _ -> [targetId]
              NextTurn -> []
              SetCurrentPlayer _ -> []

        expressionTargetIds eff =
            case eff of
              Op _ exprs -> List.concatMap expressionTargetIds exprs
              Ref targetId _ -> [targetId]
              Literal _ -> []
    in
    case schema of
        TextSchema s ->
            []

        WholeNumberSchema s ->
            []

        Group s ->
            List.concatMap targetIds s.items

        ItemList s ->
            List.concatMap targetIds s.items

        PlayerGroup s ->
            List.concatMap targetIds s.items

        Action s ->
            List.concatMap effectTargetIds s.effects

        Calculated s ->
            expressionTargetIds s.equals


idDefaultAndName : String -> TrackerSchema -> Maybe (Defaults, String)
idDefaultAndName id schema =
    case schema of
        TextSchema s ->
            Nothing

        WholeNumberSchema s ->
            if s.id == id then
                Just (s.default, s.text)

            else
                Nothing

        Group s ->
            List.head <| List.filterMap (idDefaultAndName id) s.items

        ItemList s ->
            List.head <| List.filterMap (idDefaultAndName id) s.items

        PlayerGroup s ->
            List.head <| List.filterMap (idDefaultAndName id) s.items

        Action s ->
            Nothing

        Calculated _ ->
            Nothing

idDefault : String -> TrackerSchema -> Maybe Defaults
idDefault id schema = idDefaultAndName id schema |> Maybe.map Tuple.first


fieldsById : String -> TrackerSchema -> List TrackerSchema
fieldsById id schema =
    case schema of
        TextSchema s ->
            []

        WholeNumberSchema s ->
            if s.id == id then
                [ WholeNumberSchema s ]

            else
                []

        Group s ->
            List.concatMap (fieldsById id) s.items

        ItemList s ->
            if s.id == id then
                ItemList s :: List.concatMap (fieldsById id) s.items

            else
                List.concatMap (fieldsById id) s.items

        PlayerGroup s ->
            List.concatMap (fieldsById id) s.items

        Action s ->
            []

        Calculated s ->
            if s.id == Just id then
                [ Calculated s ]

            else
                []


trackerTopLevelSchemaDecoder : Decoder TrackerTopLevelSchema
trackerTopLevelSchemaDecoder =
    Decode.map3 TrackerTopLevelSchema
        (field "name" string)
        (field "tracker" trackerSchemaDecoder)
        (Decode.map (Maybe.withDefault True) (Decode.maybe (field "turns" Decode.bool)))


type alias TrackerTopLevelSchema =
    { name : String
    , tracker : TrackerSchema
    , turns : Bool
    }
