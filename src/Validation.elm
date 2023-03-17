module Validation exposing (..)

import TrackerSchema exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)

type alias Results e w a = {
  errors : List e
  , warnings : List w
  , result : Maybe a
  }

andThen : (a -> Results e w b) -> Results e w a  -> Results e w b
andThen f res =
  case res.result of
    Just v -> merge res (f v)
    Nothing -> { errors=res.errors, warnings=res.warnings, result = Nothing }

merge : Results e w a -> Results e w b -> Results e w b
merge a b =
    {
      errors = a.errors ++ b.errors
    , warnings = a.warnings ++ b.warnings
    , result =
        case (a.result, b.result) of
          (Just _, Just res) -> Just res
          (Nothing, Just _) -> Nothing
          (Just _, Nothing) -> Nothing
          (Nothing, Nothing) -> Nothing
    }

type BadSchemaError
  = TooManyPlayerGroups { count : Int }
  | DuplicateIds { ids : List String }
  | IdsAreTargetedButNotFound { ids : List String }
  | PlayerGroupCannotBeInItemList { itemListId: String }
  | CannotSetCurrentPlayerToThisOutsideOfPlayerGroup
  | TurnsDisabledButTriedTo {attempted : String }
  | TriedToReferenceWithoutPlayerGroup { ref : CellScope }
  | CannotReferenceThisPlayerOutsideOfPlayerGroup
  | MinMustBeLessThanOrEqualToMax { id : String }
  | PlayerDefaultsOutsidePlayerRange { id : String, defaultIds : List Int, range : PlayerGroupData }
  | MoreAliasesGivenInPlayerGroupThanMaxPlayers

errorKey : BadSchemaError -> String
errorKey err =
    case err of
      TooManyPlayerGroups _ -> "too-many-player-groups"
      DuplicateIds _ -> "duplicate-ids"
      IdsAreTargetedButNotFound _ -> "targeted-ids-are-not-found"
      PlayerGroupCannotBeInItemList _ -> "player-group-in-list"
      CannotSetCurrentPlayerToThisOutsideOfPlayerGroup -> "set-current-player-outside-of-group"
      TurnsDisabledButTriedTo _ -> "tried-action-when-turns-disabled"
      TriedToReferenceWithoutPlayerGroup _ -> "player-reference-without-group"
      CannotReferenceThisPlayerOutsideOfPlayerGroup -> "this-player-reference-outside-of-group"
      MinMustBeLessThanOrEqualToMax _ -> "min-more-than-max"
      PlayerDefaultsOutsidePlayerRange _ -> "player-default-outside-of-range"
      MoreAliasesGivenInPlayerGroupThanMaxPlayers -> "more-aliases-given-than-players"

errorToString : BadSchemaError -> String
errorToString err =
    case err of
      TooManyPlayerGroups { count } -> "Only one player group can be specified. " ++ String.fromInt count ++ " given."
      DuplicateIds { ids } ->
          case ids of
              [id] -> "The following ID is duplicated: " ++ id
              is -> "The following IDs are duplicated: " ++ String.join ", " is
      IdsAreTargetedButNotFound { ids } ->
          case ids of
              [id] -> "The following ID is a target but not found in the definition: " ++ id
              is -> "The following IDs are targets but not found in the definition: " ++ String.join ", " is
      PlayerGroupCannotBeInItemList { itemListId } ->
          "A player group cannot be nested in item list: " ++ itemListId
      CannotSetCurrentPlayerToThisOutsideOfPlayerGroup ->
          "Cannot set current player to this player outside of player group"
      TurnsDisabledButTriedTo { attempted } ->
          "Turns are disabled but a component tries to " ++ attempted
      TriedToReferenceWithoutPlayerGroup { ref } ->
          "No player group exists but tried to reference " ++ scopeToString ref
      CannotReferenceThisPlayerOutsideOfPlayerGroup ->
          "'this player' cannot be referenced outside of a player group"
      MinMustBeLessThanOrEqualToMax { id } ->
          "Min must be less than or equal to max for field " ++ id
      PlayerDefaultsOutsidePlayerRange { id, defaultIds, range } ->
          "For field " ++ id ++ " player defaults for " ++ String.join "," (List.map String.fromInt defaultIds) ++ " are outside the range of [0" ++ "," ++ String.fromInt range.maxPlayers ++ "]"
      MoreAliasesGivenInPlayerGroupThanMaxPlayers ->
          "More aliases were given for players than allowed maxPlayers"

type alias ValidationResult a = Results BadSchemaError () a

emptyFail = {
    errors = []
  , warnings = []
  , result = Nothing
  }

fail : e -> Results e w a
fail err = { errors=[err], warnings=[], result=Nothing }

ensure : Bool -> e -> Results e w ()
ensure c err =
    if c then ok () else fail err

failWith : a -> e -> Results e w a
failWith v err = { errors=[err], warnings=[], result=Just v }

ok : a -> Results e w a
ok v = { errors=[], warnings=[], result=Just v }

ensureOnePlayerGroup : TrackerTopLevelSchema -> ValidationResult (Maybe PlayerGroupData)
ensureOnePlayerGroup schema =
  case findPlayerGroup schema.tracker of
    (( pgd, _ )::_::_) as vs -> { count = List.length vs } |> TooManyPlayerGroups |> failWith (Just pgd)
    (( pgd, _ )::_) -> pgd |> Just |> ok
    _ -> ok Nothing

ensureNoDupliateIds : TrackerTopLevelSchema -> ValidationResult (Set String)
ensureNoDupliateIds schema =
  let
      count k counts =
          Dict.update k (Maybe.withDefault 0 >> (+) 1 >> Just) counts
      ids =
          schema.tracker
              |> numericFieldIds
              |> List.map Tuple.first
      dupes =
          ids
              |> List.foldl count Dict.empty
              |> Dict.filter (\_ v -> v > 1)
              |> Dict.keys
  in
      case dupes of
          [] -> ok (Set.fromList ids)
          _ -> { ids = dupes } |> DuplicateIds |> failWith (Set.fromList ids)

ensureNoPlayerGroupInAnItemList : TrackerTopLevelSchema -> ValidationResult ()
ensureNoPlayerGroupInAnItemList schema =
  let
      found itemListId s =
          case s of
            ItemList list -> List.concatMap (list.id |> List.singleton |> found) list.items
            PlayerGroup _ ->
                -- Don't need to dive deeper because there should only be 1 player-group
                itemListId
            _ -> []
   in
      case schema.tracker |> found [] of
      [id] -> { itemListId = id } |> PlayerGroupCannotBeInItemList |> fail
      _ -> ok ()

itemListExists : TrackerTopLevelSchema -> Bool
itemListExists =
    let
      aux schema =
          case schema of
            TextSchema s ->
                False

            WholeNumberSchema s ->
                False

            Group s ->
                List.any aux s.items

            ItemList s ->
                True

            PlayerGroup s ->
                List.any aux s.items

            Action s ->
                False

            Calculated s ->
                False
    in
        .tracker >> aux

ensureIdScopeReferencesMakeSense : { playerGroupExists : Bool, itemListExists : Bool } -> TrackerTopLevelSchema -> ValidationResult ()
ensureIdScopeReferencesMakeSense facts tl =
  let
    ensurePlayerGroup scope =
      if facts.playerGroupExists
      then ok ()
      else { ref=scope } |> TriedToReferenceWithoutPlayerGroup |> fail

    ensureInPlayerGroup context err =
      if context.inPlayerGroup
      then ok ()
      else fail err

    ensureTurns context err =
      if context.turns
      then ok ()
      else fail err

    auxScope context scope =
        case scope of
          NonPlayer -> ok ()
          AllPlayers -> ensurePlayerGroup scope
          AllPlayersLists -> ensurePlayerGroup scope
          SpecificPlayer _ -> ensurePlayerGroup scope
          ThisPlayer -> ensurePlayerGroup scope |> merge (ensureInPlayerGroup context CannotReferenceThisPlayerOutsideOfPlayerGroup)
          CurrentPlayer -> ensurePlayerGroup scope |> merge (ensureTurns context (TurnsDisabledButTriedTo { attempted="reference current player" }))

    auxExpr context expr =
        case expr of
          Op _ exprs -> exprs |> List.map (auxExpr context) |> List.foldl merge emptyFail
          Ref _ scope -> auxScope context scope
          Literal _ -> ok ()

    auxEffect context eff =
        case eff of
          OnCells _ _ scope -> auxScope context scope
          NextTurn -> ensureTurns context (TurnsDisabledButTriedTo { attempted="move to next turn" })
          SetCurrentPlayer CurrentIsThisPlayer ->
              ensureInPlayerGroup context CannotSetCurrentPlayerToThisOutsideOfPlayerGroup
                  |> merge (ensureTurns context ({ attempted="set current player" } |> TurnsDisabledButTriedTo))

    aux context schema =
      case schema of
          TextSchema s ->
              ok ()

          WholeNumberSchema s ->
              ok ()

          Group s ->
              s.items |> List.map (aux context) |> List.foldl merge emptyFail

          ItemList s ->
              s.items |> List.map (aux { context | inItemList = True }) |> List.foldl merge emptyFail

          PlayerGroup s ->
              s.items |> List.map (aux { context | inPlayerGroup = True }) |> List.foldl merge emptyFail

          Action s ->
              s.effects |> List.map (auxEffect context) |> List.foldl merge emptyFail

          Calculated s ->
              auxExpr context s.equals
  in
    tl.tracker |> aux { inPlayerGroup = False, inItemList = False, turns = True }

ensureRanges : TrackerTopLevelSchema -> ValidationResult ()
ensureRanges =
  let
    aux playerGroupData schema =
      case schema of
          TextSchema s ->
              ok ()

          WholeNumberSchema s ->
              let
                minMax =
                  case (s.min, s.max) of
                      (Just min, Just max) ->
                        ensure (leq min max) (MinMustBeLessThanOrEqualToMax { id = s.id })
                      _ -> ok ()
                defaults =
                    case playerGroupData of
                      Nothing -> ok ()
                      Just pgd ->
                        s.default.playerDefaults
                          |> Dict.toList
                          |> List.map Tuple.first
                          |> List.filter (\p -> not (0 <= (p+1) && (p+1) <= pgd.maxPlayers))
                          |> \vs -> ensure (List.isEmpty vs) (PlayerDefaultsOutsidePlayerRange { id=s.id, defaultIds=vs, range=pgd })
              in
                minMax |> merge defaults

          Group s ->
              s.items |> List.map (aux playerGroupData) |> List.foldl merge emptyFail

          ItemList s ->
              s.items |> List.map (aux playerGroupData) |> List.foldl merge emptyFail

          PlayerGroup s ->
              let
                  minMax = ensure (s.minPlayers <= s.maxPlayers) (MinMustBeLessThanOrEqualToMax { id = "player group" })
                  aliases =
                      ensure (List.length s.defaultAliases <= s.maxPlayers) MoreAliasesGivenInPlayerGroupThanMaxPlayers
              in
                s.items
                    |> List.map (aux (Just s))
                    |> List.foldl merge minMax
                    |> merge aliases

          Action s ->
              ok ()

          Calculated s ->
              ok ()
  in
    .tracker >> aux Nothing

validateSchema : TrackerTopLevelSchema -> ValidationResult ()
validateSchema schema =
  let
      ensureRefs maybePg =
          let
            itemList = itemListExists schema
          in
            ensureIdScopeReferencesMakeSense { playerGroupExists = maybePg /= Nothing, itemListExists = itemList } schema
      groupsAndRefs = ensureOnePlayerGroup schema |> andThen ensureRefs
      ensureIds = ensureNoDupliateIds schema
      ensureTargetsHitRealIds =
          case ensureIds.result of
              Nothing -> ok ()
              Just actuals ->
                  let
                      targets = Set.fromList (targetIds schema.tracker)
                      inTargetButNotActual = Set.diff targets actuals
                  in
                      if Set.isEmpty inTargetButNotActual
                      then ok ()
                      else { ids = Set.toList inTargetButNotActual } |> IdsAreTargetedButNotFound |> fail
  in
      groupsAndRefs
          |> merge ensureIds
          |> merge ensureTargetsHitRealIds
          |> merge (ensureNoPlayerGroupInAnItemList schema)
          |> merge (ensureRanges schema)
