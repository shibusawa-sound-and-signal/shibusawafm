module Main exposing (Model(..), Msg(..), init, main, placeholderCard, update, view)

import Browser
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (attribute, class, src, style)
import Http
import Json.Decode exposing (Decoder, field, float, int, list, map2, map3, map6, nullable, string)
import List exposing (repeat)
import Markdown

showGraph = False

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


type alias Comment =
    { headline : String
    , excerpt : String
    }


type alias Track =
    { id : String
    , title : String
    , artists : List Artist
    , album : Album
    , comment : Maybe Comment
    , features : TrackFeatures
    }


type alias TrackFeatures =
    { danceability : Float
    , energy : Float
    , tempo : Float
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


commentDecoder : Decoder Comment
commentDecoder =
    map2 Comment (field "headline" string) (field "excerpt" string)


trackDecoder : Decoder Track
trackDecoder =
    map6 Track
        (field "id" string)
        (field "title" string)
        (field "artists" (list artistDecorder))
        (field "album" albumDecoder)
        (field "comment" (nullable commentDecoder))
        (map3 TrackFeatures
            (field "danceability" float)
            (field "energy" float)
            (field "tempo" float)
        )


trackListDecoder : Decoder TrackList
trackListDecoder =
    list trackDecoder


getTrackList =
    Http.get
        { url = "/playlist/627twzacY3mbvUUySz0qPD"
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


card cardTitle artists imageSrc headline excerpt =
    div
        [ class "col-md-6" ]
        [ div
            [ class "row"
            , class "mb-4"
            , class "shadow-sm"
            , class "no-gutters"
            , class "border"
            , class "rounded"
            ]
            [ div [ class "col-md-8", class "p-2" ]
                [ div [] [ text cardTitle ]
                , div [ class "text-muted" ] [ text artists ]
                , div [] <| Markdown.toHtml Nothing headline
                ]
            , div [ class "col-md-4" ]
                [ img [ src imageSrc, attribute "width" "100%" ] []
                ]
            , div
                [ class "col-md-12", class "p-2" ]
              <|
                Markdown.toHtml Nothing excerpt
            ]
        ]


placeholderCard =
    card "~~~~~~~~~~" "~~~~~~~~~~" placeholderPng "_~~~~~~~~~~_" "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"


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
        (Maybe.withDefault "" <| Maybe.map .headline track.comment)
        (Maybe.withDefault "" <| Maybe.map .excerpt track.comment)


cards : Model -> List (Html Msg)
cards model =
    case model of
        Init ->
            repeat 12 placeholderCard

        Ready tracks ->
            List.map trackCard tracks


graph : String -> String -> Float -> List Float -> Html Msg
graph colorString _ domain values =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "align-items" "flex-end"
        , style "height" "50px"
        , style "width" "100%"
        ]
        (List.map
            (\v ->
                span
                    [ style "height" (String.fromFloat ((v / domain) * 100) ++ "%")
                    , style "background-color" colorString
                    , style "flex-grow" "1"
                    , style "border-left" "1px solid black"
                    ]
                    []
            )
            values
        )


visualization : Model -> List (Html Msg)
visualization model =
    case model of
        Init ->
            []

        Ready tracks ->
            if showGraph then
                [ graph "green" "energy" 1.0 <| List.map (\t -> t.features.energy) tracks
                , graph "red" "tempo" 200.0 <| List.map (\t -> t.features.tempo) tracks
                , graph "blue" "danceability" 1.0 <| List.map (\t -> t.features.danceability) tracks
                ]
            else
                []


view model =
    div [ class "py-5" ]
        [ div [ class "row" ] <| visualization model
        , div [ class "row" ] <| cards model
        ]
