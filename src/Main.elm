module Main exposing (Model(..), Msg(..), init, main, placeholderCard, update, view)

import Browser
import Html exposing (Html, div, em, img, input, section, span, text, textarea)
import Html.Attributes exposing (attribute, class, src, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, float, int, list, map2, map3, map6, nullable, string)
import List exposing (repeat)

-- TODO: render background of modal as translucent
-- TODO: position modal centered
-- TODO: on edit, append a work item to the work queue (post this track comment to the server)
-- TODO: if there are items in the work queue, check to see when they are ready, and save them
-- TODO: if there is another pending track save of the same track, overwrite it

showGraph =
    False


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


type UIMode
    = Default
    | Editing Track


type Model
    = Init
    | Ready TrackList UIMode


editing : Model -> Track -> Model
editing model track =
    case model of
        Ready tracks _ ->
            Ready tracks (Editing track)

        _ ->
            model


type Msg
    = Loaded (Result Http.Error TrackList)
    | NoOp
    | OpenEditor Track
    | EditTrack Track


changeTrack : Model -> Track -> Model
changeTrack model track =
    case model of
        Ready tracks _ ->
            Ready tracks (Editing track)

        _ ->
            model


update msg model =
    case msg of
        Loaded (Ok tracks) ->
            ( Ready tracks Default, Cmd.none )

        OpenEditor track ->
            ( editing model track, Cmd.none )

        EditTrack track ->
            ( editing (changeTrack model track) track, Cmd.none )

        _ ->
            ( model, Cmd.none )


placeholderPng =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAEsCAYAAAB5fY51AAAC+0lEQVR42u3UQREAAAQAQVLJon8QcjC7Ee5xGV0TAAekYQGGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWYFiGBRgWgGEBhgVgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFiAYRkWYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQGGZViAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBRiWDIBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBhmVYgGEBGBZgWACGBWBYgGEBGBaAYQGGBWBYAIYFGBaAYQEYFmBYAIYFYFiAYQEYFoBhAYYFYFgAhgUYFoBhARgWYFgAhgUYlmEBhgVgWIBhARgWgGEBhgVgWACGBRgWgGEBGBbw2QIT2dTQOj7WUQAAAABJRU5ErkJggg=="


card cardTitle artists imageSrc headline excerpt clickMsg =
    span
        [ class "p-1", onClick clickMsg ]
        [ img [ src imageSrc, attribute "width" "40px", style "vertical-align" "baseline" ] []
        , div [ style "display" "inline-block", class "px-2" ]
            [ div [ class "p-2" ] [ text cardTitle ]
            , div [ class "text-muted", class "p-2" ] [ text artists ]
            ]
        , span [ style "font-size" "24px", class "px-2" ] [ em [] [ Html.text headline ] ]
        , span [ class "text-muted", class "px-2", style "line-height" "3" ] [ Html.text excerpt ]
        ]


placeholderCard =
    card "\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}" "\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}" placeholderPng "\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}" "\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}" NoOp


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
        (Maybe.withDefault placeholderPng <| Maybe.map .src (mediumImage track.album.images))
        (Maybe.withDefault "" <| Maybe.map .headline track.comment)
        (Maybe.withDefault "" <| Maybe.map .excerpt track.comment)
        (OpenEditor track)


cards : Model -> List (Html Msg)
cards model =
    case model of
        Init ->
            repeat 12 placeholderCard

        Ready tracks _ ->
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

        Ready tracks _ ->
            if showGraph then
                [ graph "green" "energy" 1.0 <| List.map (\t -> t.features.energy) tracks
                , graph "red" "tempo" 200.0 <| List.map (\t -> t.features.tempo) tracks
                , graph "blue" "danceability" 1.0 <| List.map (\t -> t.features.danceability) tracks
                ]

            else
                []


setCommentHeadline : Track -> String -> Track
setCommentHeadline track newHeadline =
    track.comment
        |> Maybe.withDefault { headline = "", excerpt = "" }
        |> (\comment -> { comment | headline = newHeadline })
        |> (\comment -> { track | comment = Just comment })


setCommentExcerpt : Track -> String -> Track
setCommentExcerpt track newExcerpt =
    track.comment
        |> Maybe.withDefault { headline = "", excerpt = "" }
        |> (\comment -> { comment | excerpt = newExcerpt })
        |> (\comment -> { track | comment = Just comment })


editingView : Track -> Html Msg
editingView track =
    div
        [ style "position" "fixed"
        , style "background-color" "green"
        , style "height" "100vh"
        , style "width" "100vw"
        , style "top" "0"
        , style "left" "0"
        ]
        [ section []
            [ input [ onInput (setCommentHeadline track >> EditTrack), value <| Maybe.withDefault "" <| Maybe.map .headline track.comment ] []
            , textarea [ onInput (setCommentExcerpt track >> EditTrack), value <| Maybe.withDefault "" <| Maybe.map .excerpt track.comment ] []
            ]
        ]


modalContainer : Model -> Html Msg
modalContainer model =
    case model of
        Ready _ (Editing track) ->
            editingView track

        _ ->
            Html.text ""


view model =
    div [ class "py-5" ]
        [ div [ class "row" ] <| visualization model
        , div [] <| cards model
        , modalContainer model
        ]
