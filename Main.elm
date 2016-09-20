module Main exposing (..)

import Model exposing (..)
import View exposing (..)
import Time exposing (Time, second)
import Random exposing (..)
import Html.App as App


main : Program Never
main =
    App.program
        { init = ( emptyWorld 10, randomStateGenerator 10 )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Seed states ->
            ( initModel model states, Cmd.none )

        Tick ->
            if model.paused then
                ( model, Cmd.none )
            else
                ( tick model, Cmd.none )

        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        NewWorld ->
            ( model, randomStateGenerator model.dimension )

        ToggleState cell ->
            ( toggleState model cell, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second (\_ -> Tick)


randomStateGenerator : Int -> Cmd Msg
randomStateGenerator dimension =
    Random.generate
        Seed
        (Random.list (dimension * dimension)
            (Random.int 1 2
                |> Random.map
                    (\n ->
                        if n == 1 then
                            Alive
                        else
                            Dead
                    )
            )
        )
