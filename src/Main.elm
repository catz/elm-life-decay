module Main exposing (colorAlive, colorDead, main, update)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Color.Interpolate
import Html exposing (Html, span, text)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Task


maxHealth : Int
maxHealth =
    100


cellSize : Float
cellSize =
    20


gutterSize : Int
gutterSize =
    3


decay : Int
decay =
    2


colorAlive : Color.Color
colorAlive =
    Color.rgb255 255 68 17


colorDead : Color.Color
colorDead =
    Color.rgb255 204 204 204



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { generation : Int
    , board : Array Int
    , viewport : Maybe Viewport
    , width : Int
    , height : Int
    , run : Bool
    , delta : Float
    }


initialModel : Model
initialModel =
    { generation = 0
    , board = Array.empty
    , viewport = Nothing
    , width = 0
    , height = 0
    , run = False
    , delta = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Task.perform GotViewport Browser.Dom.getViewport )



-- UPDATE


type Msg
    = InitBoard (List Int)
    | GotViewport Viewport
    | Next Float
    | Run


getPosition : Int -> Int -> Int -> Int
getPosition width col row =
    width * row + col


getCoordinates : Int -> Int -> ( Int, Int )
getCoordinates width position =
    ( remainderBy width position, position // width )


surroundingPositions : Int -> Int -> List Int
surroundingPositions width position =
    let
        ( cellX, cellY ) =
            getCoordinates width position
    in
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]
        |> List.map
            (\( x, y ) ->
                if (cellX - x) < 0 || (cellY - y) < 0 || (cellX - x) > width - 1 then
                    Nothing

                else
                    Just <| getPosition width (cellX - x) (cellY - y)
            )
        |> List.filterMap identity


getNeighborsCount : Int -> Int -> Array Int -> Int
getNeighborsCount width position board =
    surroundingPositions width position
        |> List.filterMap
            (\p ->
                Maybe.map
                    (\health ->
                        if health == maxHealth then
                            1

                        else
                            0
                    )
                    (Array.get p board)
            )
        |> List.sum


generate : Model -> Model
generate model =
    let
        nextBoard =
            model.board
                |> Array.indexedMap
                    (\position value ->
                        let
                            neighborsCount =
                                getNeighborsCount model.width position model.board

                            nextValue =
                                if value == maxHealth && neighborsCount < 2 then
                                    -- Loneliness
                                    value - decay

                                else if value == maxHealth && neighborsCount > 3 then
                                    -- Overpopulation
                                    value - decay

                                else if value < maxHealth && neighborsCount == 3 || value == maxHealth then
                                    -- Reproduction
                                    maxHealth

                                else
                                    value - decay
                        in
                        clamp 0 maxHealth nextValue
                    )
    in
    { model | board = nextBoard }


possibleStates : Int -> Generator (List Int)
possibleStates n =
    Random.list n (Random.int 0 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitBoard randomIntegers ->
            ( { model
                | board =
                    randomIntegers
                        |> List.map ((*) maxHealth)
                        |> Array.fromList
              }
            , Cmd.none
            )

        GotViewport viewport ->
            let
                width =
                    viewport.viewport.width

                height =
                    viewport.viewport.height

                columns =
                    floor (width / cellSize) - 2

                rows =
                    floor (height / cellSize) - 2
            in
            ( { model
                | viewport = Just viewport
                , width = columns
                , height = rows
              }
            , Random.generate InitBoard (possibleStates (columns * rows))
            )

        Next delta ->
            if model.delta + delta >= 100 then
                ( generate { model | generation = model.generation + 1, delta = 0 }, Cmd.none )

            else
                ( { model | delta = model.delta + delta }, Cmd.none )

        Run ->
            ( { model | run = not model.run }, Cmd.none )



-- VIEW


renderSquare : Color.Color -> Canvas.Point -> Renderable
renderSquare color ( x, y ) =
    shapes [ fill color ]
        [ rect ( x + toFloat gutterSize, y + toFloat gutterSize )
            (cellSize - toFloat gutterSize)
            (cellSize - toFloat gutterSize)
        ]


renderSquares : Model -> List Renderable
renderSquares model =
    model.board
        |> Array.indexedMap
            (\position health ->
                let
                    ( cellX, cellY ) =
                        getCoordinates model.width position

                    color =
                        if health == maxHealth then
                            colorAlive

                        else
                            Color.Interpolate.interpolate Color.Interpolate.RGB
                                Color.black
                                colorDead
                                (toFloat health / 100)
                in
                renderSquare color ( toFloat cellX * cellSize + cellSize, toFloat cellY * cellSize + cellSize )
            )
        |> Array.toList


clearScreen width height =
    [ shapes
        [ fill Color.black ]
        [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
    ]


view : Model -> Html Msg
view model =
    case model.viewport of
        Just viewport ->
            let
                width =
                    floor viewport.viewport.width

                height =
                    floor viewport.viewport.height
            in
            Html.div []
                [ Canvas.toHtml ( width, height )
                    [ onClick Run ]
                    (clearScreen width height
                        ++ renderSquares model
                    )
                , Html.button [ onClick Run ] [ Html.text "Toggle run" ]
                , Html.button [ onClick <| Next 100 ] [ Html.text "Next" ]
                , span []
                    [ text <|
                        String.fromInt model.generation
                            ++ " "
                            ++ String.fromInt (model.height * model.width)
                    ]
                ]

        Nothing ->
            text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.run then
        onAnimationFrameDelta Next

    else
        Sub.none
