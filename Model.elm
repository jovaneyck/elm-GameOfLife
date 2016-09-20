module Model exposing (..)

import Util exposing (zip)


type alias Model =
    { dimension : Int, world : World, paused : Bool }


type alias World =
    List PositionedCell


type alias PositionedCell =
    { state : State
    , location : Location
    }


type State
    = Alive
    | Dead


type alias Location =
    { x : Int, y : Int }


emptyWorld : Int -> Model
emptyWorld dimension =
    { dimension = dimension
    , world = []
    , paused = False
    }


initModel : Model -> List State -> Model
initModel model states =
    let
        allCoordinates =
            [1..model.dimension]
                |> List.concatMap
                    (\x -> [1..model.dimension] |> List.map (\y -> { x = x, y = y }))

        cells =
            allCoordinates
                |> zip states
                |> List.map (\( state, coords ) -> { location = coords, state = state })
    in
        { model | world = cells }


tick : Model -> Model
tick model =
    { model
        | world =
            model.world |> List.map (\cell -> tickCell cell model.world)
    }


tickCell : PositionedCell -> World -> PositionedCell
tickCell cell world =
    let
        neighbours =
            findNeighboursFor cell.location world

        nbLivingNeighbours =
            neighbours
                |> List.filter (\{ state } -> state == Alive)
                |> List.length
    in
        applyRules nbLivingNeighbours cell


findNeighboursFor : Location -> List PositionedCell -> List PositionedCell
findNeighboursFor coordinates cells =
    let
        deltas =
            [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ), ( -1, 0 ), ( 1, 0 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]

        coordinatesToCheck =
            deltas
                |> List.map
                    (\( dx, dy ) ->
                        { x = coordinates.x + dx, y = coordinates.y + dy }
                    )
    in
        cells
            |> List.filter
                (\{ location } ->
                    coordinatesToCheck
                        |> List.member { x = location.x, y = location.y }
                )


applyRules : Int -> PositionedCell -> PositionedCell
applyRules nbLivingNeighbours cell =
    let
        newState =
            case cell.state of
                Dead ->
                    if nbLivingNeighbours == 3 then
                        Alive
                    else
                        Dead

                Alive ->
                    case nbLivingNeighbours of
                        2 ->
                            Alive

                        3 ->
                            Alive

                        _ ->
                            Dead
    in
        { cell | state = newState }


flip : State -> State
flip state =
    case state of
        Alive ->
            Dead

        Dead ->
            Alive


toggleState : Model -> PositionedCell -> Model
toggleState model cell =
    let
        toggled =
            { cell | state = flip cell.state }

        others =
            model.world
                |> List.filter (\c -> c /= cell)
    in
        { model | world = toggled :: others }


kill : PositionedCell -> PositionedCell
kill cell =
    { cell | state = Dead }


killCells : Model -> Model
killCells model =
    { model | world = model.world |> List.map kill }
