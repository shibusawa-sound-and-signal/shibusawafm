module Main exposing (Model(..), Msg(..), init, main, placeholderCard, update, view)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Http
import List exposing (repeat)
import Json.Decode exposing (Decoder, field, list, map2, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

type alias Track =
       {
            id: String,
            title: String
       }


trackListDecoder : Decoder (List Track)
trackListDecoder =
  list <| map2 Track (field "id" string) (field "title" string)

getTrackList = Http.get
              { url = "/annotated-track-list/627twzacY3mbvUUySz0qPD"
              , expect = Http.expectJson Loaded trackListDecoder
              }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init, getTrackList )


type Model
    = Init


type Msg
    = Loaded (Result Http.Error (List Track))


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
