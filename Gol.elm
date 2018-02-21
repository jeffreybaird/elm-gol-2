module Gol exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes exposing (width, height, viewBox)
import Svg.Events
import Random.Pcg exposing (Seed, initialSeed, step, int)
import Debug


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { height : Int, width : Int, grid : FlatGrid }


type alias Grid =
    List Row


type alias FlatGrid =
    Row


type alias Neighbors =
    Row


type alias Row =
    List Cell


type alias Cell =
    { status : Status, position : Coordinate }


type alias RenderedCell =
    Svg.Svg Msg


type alias RenderedRow =
    List RenderedCell


type alias Coordinate =
    ( Int, Int )


type Community
    = Reproduction
    | Overpopulation
    | Starvation
    | LiveOn
    | StayDead


seed : Seed
seed =
    initialSeed 126781231212


model : Model
model =
    let
        h =
            10

        w =
            10

        grid =
            svgGridBuilder h w seed []
                |> flattenedSvgGrid
    in
        { height = h, width = w, grid = grid }


stepGrid : Model -> Model
stepGrid model =
    let
        newGrid =
            List.map (resetCellStatus model.grid) model.grid
    in
        { model | grid = newGrid }


resetCellStatus : FlatGrid -> Cell -> Cell
resetCellStatus grid cell =
    let
        community =
            nextGenerationStatus grid cell

        newStatus =
            case community of
                Starvation ->
                    Dead

                LiveOn ->
                    Alive

                Overpopulation ->
                    Dead

                Reproduction ->
                    Alive

                StayDead ->
                    Dead
    in
        { cell | status = newStatus }


nextGenerationStatus : FlatGrid -> Cell -> Community
nextGenerationStatus grid cell =
    let
        sizeOfHood =
            neighbors cell grid
                |> List.length
    in
        if cell.status == Alive then
            case sizeOfHood of
                0 ->
                    Starvation

                1 ->
                    Starvation

                2 ->
                    LiveOn

                3 ->
                    LiveOn

                _ ->
                    Overpopulation
        else
            case sizeOfHood of
                3 ->
                    Reproduction

                _ ->
                    StayDead


neighbors : Cell -> FlatGrid -> Neighbors
neighbors cell grid =
    let
        isANeighbor : Cell -> Bool
        isANeighbor c =
            positionOfNeighbors cell.position
                |> List.member c.position
    in
        List.filter isANeighbor grid
            |> List.filter (\c -> c.status == Alive)


positionOfNeighbors : Coordinate -> List Coordinate
positionOfNeighbors coord =
    let
        ( x, y ) =
            coord
    in
        [ ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x + 1, y + 1 )
        , ( x, y + 1 )
        , ( x - 1, y + 1 )
        ]



-- UPDATE


type Msg
    = Click Coordinate
    | Step


type Status
    = Alive
    | Dead


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click position ->
            { model | grid = (toggleClickedCell position model.grid) }

        Step ->
            stepGrid model


toggleStatus : Status -> Status
toggleStatus status =
    if status == Alive then
        Dead
    else
        Alive


toggleClickedCell : Coordinate -> Row -> Row
toggleClickedCell position cells =
    List.map
        (\c ->
            if c.position == position then
                { c | status = (toggleStatus c.status) }
            else
                c
        )
        cells



-- VIEW


renderCell : Cell -> Svg.Svg Msg
renderCell cell =
    let
        ( x, y ) =
            cell.position
    in
        cellColor cell
            |> svgSquare x y


cellColor : Cell -> String
cellColor cell =
    if cell.status == Alive then
        "blue"
    else
        "white"


renderGrid : FlatGrid -> RenderedRow
renderGrid row =
    List.map renderCell row


svgSquare : Int -> Int -> String -> RenderedCell
svgSquare x y color =
    let
        newX =
            x
                * 10
                |> toString

        newY =
            y
                * 10
                |> toString
    in
        Svg.rect
            [ width "10"
            , height "10"
            , Svg.Attributes.fill color
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "0.5"
            , Svg.Attributes.x newX
            , Svg.Attributes.y newY
            , Svg.Events.onClick (Click ( x, y ))
            ]
            []


svgRowBuilder : Int -> Int -> Seed -> Row -> Row
svgRowBuilder length rowNumber seed row =
    if length == 0 then
        row
    else
        let
            ( randomNumber, seed1 ) =
                step (int 0 1) seed

            status =
                if randomNumber == 0 then
                    Dead
                else
                    Alive

            newRow =
                ({ status = status, position = ( length, rowNumber ) }) :: row
        in
            svgRowBuilder (length - 1) rowNumber seed1 newRow


svgGridBuilder : Int -> Int -> Seed -> Grid -> Grid
svgGridBuilder height width seed grid =
    if height == 0 then
        grid
    else
        let
            ( _, seed1 ) =
                step (int 0 1) seed

            newGrid =
                (svgRowBuilder width height seed1 []) :: grid
        in
            svgGridBuilder (height - 1) width seed1 newGrid


flattenedSvgGrid : Grid -> FlatGrid
flattenedSvgGrid grid =
    List.foldr (++) [] grid


view : Model -> Html Msg
view model =
    let
        grid =
            model.grid
                |> renderGrid
    in
        div []
            [ div []
                [ Svg.svg [ width "500", height "500", viewBox "10 10 100 100" ] (grid) ]
            , button [ onClick Step ] [ text "Step" ]
            ]
