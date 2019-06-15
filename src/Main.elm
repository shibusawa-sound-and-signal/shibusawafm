module Main exposing (Model(..), Msg(..), init, main, placeholderCard, update, view)

import Browser
import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (attribute, class, property, src, width)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, map4, string)
import Json.Encode as Encode
import List exposing (repeat)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Artist =
    { id : String
    , name : String
    }


type alias Image =
    { src : String
    , height : Int
    , width : Int
    }


type alias Album =
    { id : String
    , name : String
    , images : List Image
    }


type alias Track =
    { id : String
    , title : String
    , artists : List Artist
    , album : Album
    }


type alias TrackList =
    List Track


artistDecorder : Decoder Artist
artistDecorder =
    map2 Artist (field "id" string) (field "name" string)


imageDecoder : Decoder Image
imageDecoder =
    map3 Image (field "src" string) (field "height" int) (field "width" int)


albumDecoder : Decoder Album
albumDecoder =
    map3 Album (field "id" string) (field "name" string) (field "images" (list imageDecoder))


trackListDecoder : Decoder TrackList
trackListDecoder =
    list <| map4 Track (field "id" string) (field "title" string) (field "artists" (list artistDecorder)) (field "album" albumDecoder)


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


placeholderPng =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAEsCAYAAAB5fY51AAAC+0lEQVR42u3UQREAAAQAQVLJon8QcjC7Ee5xGV0TAAekYQGGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWYFiGBRgWgGEBhgVgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFiAYRkWYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQGGZViAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBRiWDIBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBhmVYgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgUYlmEBhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBbw2QIT2dTQOj7WUQAAAABJRU5ErkJggg=="


card cardTitle artists imageSrc =
    div [ class "col-md-3" ]
        [ div [ class "card", class "mb-4", class "shadow-sm" ]
            [ img [ src imageSrc, attribute "width" "100%" ] []
            , div [ class "card-body" ]
                [ p [ class "card-text" ] [ text cardTitle ]
                , p [ class "card-text" ] [ text artists ]
                , div [ class "d-flex", class "justify-content-between", class "align-items-center" ]
                    []
                ]
            ]
        ]


placeholderCard =
    card "~~~~~~~~~~" "~~~~~~~~~~" placeholderPng


artistListString : List Artist -> String
artistListString artists =
    String.join ", " <| List.map .name artists


mediumImage : List Image -> Maybe Image
mediumImage =
    List.sortBy .height
        >> List.reverse
        >> List.drop 1
        >> List.head


trackCard track =
    card
        track.title
        (artistListString track.artists)
        (Maybe.withDefault "" <| Maybe.map .src (mediumImage track.album.images))


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
