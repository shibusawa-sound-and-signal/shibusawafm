module Main exposing (Model(..), Msg(..), init, main, placeholderCard, update, view)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, list, map2, string)
import List exposing (repeat)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Track =
    { id : String
    , title : String
    }


type alias TrackList =
    List Track


trackListDecoder : Decoder TrackList
trackListDecoder =
    list <| map2 Track (field "id" string) (field "title" string)


getTrackList =
    Http.get
        { url = "/annotated-track-list/627twzacY3mbvUUySz0qPD"
        , expect = Http.expectJson Loaded trackListDecoder
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init, getTrackList )


type Model
    = Init
    | Ready TrackList


type Msg
    = Loaded (Result Http.Error TrackList)


update msg model =
    case msg of
        Loaded (Ok tracks) ->
            ( Ready tracks, Cmd.none )

        _ ->
            ( model, Cmd.none )


card cardTitle =
    div [ class "col-md-4" ]
        [ div [ class "card", class "mb-4", class "shadow-sm" ]
            [ div [ class "card-body" ]
                [ p [ class "card-text" ] [ text cardTitle ]
                , div [ class "d-flex", class "justify-content-between", class "align-items-center" ] []
                ]
            ]
        ]


placeholderCard =
    card "~~~~~~~~~~"


trackCard track =
    card track.title


cards : Model -> List (Html Msg)
cards model =
    case model of
        Init ->
            repeat 12 placeholderCard

        Ready tracks ->
            List.map trackCard tracks


view model =
    div [ class "album", class "py-5", class "bg-light" ]
        [ div [ class "container" ]
            [ div [ class "row" ] <| cards model ]
        ]
