module Main exposing (Model(..), Msg(..), init, main, placeholderCard, update, view)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import List exposing (repeat)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( Init, Cmd.none )


type Model
    = Init


type Msg
    = Msg


update msg model =
    ( model, Cmd.none )


placeholderCard =
    div [ class "col-md-4" ]
        [ div [ class "card", class "mb-4", class "shadow-sm" ]
            [ div [ class "card-body" ]
                [ p [ class "card-text" ] [ text "~~~~~~~~~~" ]
                , div [ class "d-flex", class "justify-content-between", class "align-items-center" ] []
                ]
            ]
        ]


view model =
    div [ class "album", class "py-5", class "bg-light" ]
        [ div [ class "container" ]
            [ div [ class "row" ] <| repeat 8 placeholderCard ]
        ]
