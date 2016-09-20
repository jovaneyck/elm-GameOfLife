module View exposing (view, Msg(..))

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = Seed (List State)
    | Tick
    | TogglePause
    | NewWorld


print : State -> String
print state =
    case state of
        Alive ->
            "👾"

        Dead ->
            "👻"


row : List PositionedCell -> Int -> Html Msg
row cells rowNumber =
    cells
        |> List.filter (\r -> r.location.x == rowNumber)
        |> List.sortBy (\r -> r.location.y)
        |> List.map (\r -> print r.state)
        |> List.map text
        |> div []


view : Model -> Html Msg
view model =
    let
        nbRows =
            model.world
                |> List.map (\c -> c.location.x)
                |> List.maximum
                |> Maybe.withDefault 0

        pauseMessage =
            if model.paused then
                "Unpause"
            else
                "Pause"
    in
        div []
            [ div
                [ style [ ( "font-size", "4em" ) ] ]
                ([0..nbRows]
                    |> List.map (row model.world)
                )
            , button [ onClick TogglePause ]
                [ text pauseMessage
                ]
            , button [ onClick NewWorld ]
                [ text "Seed new world"
                ]
            ]
