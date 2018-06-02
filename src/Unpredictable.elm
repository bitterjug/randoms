module Unpredictable exposing (..)

import Html exposing (Html, program)
import Random exposing (Generator)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
infixl 0 =>


main : Program Never Grid Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


type alias Grid =
    List (List Int)


type alias Msg =
    Grid


cols : Int
cols =
    10


rows : Int
rows =
    10


init : ( Grid, Cmd Msg )
init =
    [ [ 1, 2, 3, 4, 5, 6, 7 ]
    , [ 8, 9, 10, 11, 12, 13, 14 ]
    , [ 15, 16, 17, 18, 19, 20 ]
    ]
        => Random.generate identity gridgen


gridgen : Generator Grid
gridgen =
    Random.list cols <|
        Random.list rows <|
            Random.int 0 9


update : Grid -> Grid -> ( Grid, Cmd Msg )
update newGrid model =
    newGrid => Cmd.none


view : Grid -> Html.Html Msg
view =
    List.map viewRow
        >> Html.table []


viewRow : List Int -> Html Msg
viewRow =
    List.map
        (toString
            >> Html.text
            >> List.singleton
            >> Html.td []
        )
        >> Html.tr []
