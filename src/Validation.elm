module Validation exposing (..)

import TrackerSchema exposing (..)
import Dict exposing (Dict)

type alias Results e w a = {
  errors : List e
  , warnings : List w
  , result : Maybe a
  }

type BadSchemaError
  = TooManyPlayerGroups { count : Int }
  | DuplicateIds { ids : List String }

type alias ValidationResult a = Results BadSchemaError () a

fail : e -> Results e w a
fail err = { errors=[err], warnings=[], result=Nothing }

ok : a -> Results e w a
ok v = { errors=[], warnings=[], result=Just v }

ensureOnePlayerGroup : TrackerTopLevelSchema -> ValidationResult ()
ensureOnePlayerGroup schema =
  case findPlayerGroup schema.tracker of
    (_::_::_) as vs -> { count = List.length vs } |> TooManyPlayerGroups |> fail
    _ -> ok ()

ensureNoDupliateIds : TrackerTopLevelSchema -> ValidationResult ()
ensureNoDupliateIds schema =
  let
      count k counts =
          Dict.update k (Maybe.withDefault 0 >> (+) 1 >> Just) counts
      dupes =
          schema.tracker
              |> numericFieldIds
              |> List.map Tuple.first
              |> List.foldl count Dict.empty
              |> Dict.filter (\_ v -> v > 1)
              |> Dict.keys
  in
      case dupes of
          [] -> ok ()
          _ -> { ids = dupes } |> DuplicateIds  |> fail

-- No player group in an item list
-- ID scope references make sense
-- No cyclical references
-- max/min coherency
-- Player group aliases are longer than max
-- Per-player defaults reference in range
-- Turns disabled but player-affecting components exist
