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
    | ToggleState PositionedCell
    | KillCells


print : PositionedCell -> Html Msg
print cell =
    let
        ( txt, color ) =
            case cell.state of
                Alive ->
                    ( "👾", "green" )

                Dead ->
                    ( "👻", "grey" )
    in
        span
            [ style [ ( "color", color ) ]
            , onClick (ToggleState cell)
            ]
            [ text txt ]


row : List PositionedCell -> Int -> Html Msg
row cells rowNumber =
    cells
        |> List.filter (\c -> c.location.x == rowNumber)
        |> List.sortBy (\c -> c.location.y)
        |> List.map print
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
            [ button [ onClick TogglePause ]
                [ text pauseMessage
                ]
            , button [ onClick KillCells ]
                [ text "Kill all cells"
                ]
            , button [ onClick NewWorld ]
                [ text "Seed new world"
                ]
            , div
                [ style [ ( "font-size", "4em" ) ] ]
                ([0..nbRows]
                    |> List.map (row model.world)
                )
            ]
