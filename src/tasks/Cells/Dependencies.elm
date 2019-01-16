module Tasks.Cells.Dependencies exposing
    ( Dependencies
    , empty
    , getDependentsOn
    , updateDependenciesFor
    )

import Dict exposing (Dict)
import Set exposing (Set)
import Tasks.Cells.Position as Position exposing (Position)


type alias Dependencies =
    Dict String (Set String)


empty : Dependencies
empty =
    Dict.empty


getDependentsOn : Position -> Dependencies -> List Position
getDependentsOn position dependencies =
    Dict.get (Position.toString position) dependencies
        |> Maybe.withDefault Set.empty
        |> Set.toList
        |> List.filterMap Position.fromString


updateDependenciesFor : Position -> { old : List Position, new : List Position } -> Dependencies -> Dependencies
updateDependenciesFor position positionDeps dependencies =
    let
        removeDep dependency ds =
            Dict.update (Position.toString dependency)
                (\maybeDependents ->
                    maybeDependents
                        |> Maybe.map (\dependents -> Set.remove (Position.toString position) dependents)
                        |> Maybe.andThen
                            (\dependents ->
                                if Set.isEmpty dependents then
                                    Nothing

                                else
                                    Just dependents
                            )
                )
                ds

        addDep dependency ds =
            Dict.update (Position.toString dependency)
                (\maybeDependents ->
                    maybeDependents
                        |> Maybe.withDefault Set.empty
                        |> Set.insert (Position.toString position)
                        |> Just
                )
                ds

        dependenciesWithoutOld =
            List.foldl removeDep dependencies positionDeps.old
    in
    List.foldl addDep dependenciesWithoutOld positionDeps.new
