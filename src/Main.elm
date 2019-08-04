module Main exposing (Model(..), Msg(..), init, main, placeholderCard, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, em, img, input, section, span, text, textarea)
import Html.Attributes exposing (attribute, class, src, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, Value, decodeValue, field, float, int, list, map, map2, map3, map6, nullable, string, succeed)
import Json.Encode as Encode
import List exposing (repeat)
import Task
import Time exposing (Posix)


showGraph =
    False


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
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


getTrackList playlistId=
    Http.get
        { url = "/playlist/" ++ playlistId
        , expect = Http.expectJson Loaded trackListDecoder
        }


encodeComment : Comment -> Encode.Value
encodeComment comment =
    Encode.object
        [ ( "excerpt", Encode.string comment.excerpt )
        , ( "headline", Encode.string comment.headline )
        ]


postComment comment trackId =
    Http.post
        { url = "/comment/" ++ trackId
        , body = Http.jsonBody (encodeComment comment)
        , expect = Http.expectJson (SavedComment trackId) (succeed ())
        }

type alias Config =
    {
        defaultPlaylistId: String
    }

configDecoder : Decoder Config
configDecoder =
    map Config
        (field "defaultPlaylistId" string)


init : Value -> ( Model, Cmd Msg )
init json =
    case decodeValue configDecoder json of
        Ok config ->
            ( Init, getTrackList config.defaultPlaylistId)
        _ -> (Errored, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Ready { pendingCommands } ->
            if Dict.values pendingCommands |> List.filter (.started >> not) |> List.isEmpty |> not then
                Time.every 2000 Tick

            else
                Sub.none

        _ ->
            Sub.none


type UIMode
    = Default
    | Editing Track


type alias PendingCommand =
    { time : Posix
    , comment : Comment
    , trackId : String
    , started : Bool
    }


type alias ReadyModel =
    { tracks : List Track
    , uiMode : UIMode
    , pendingCommands : Dict String PendingCommand
    }


type alias TrackId =
    String


type Model
    = Init
    | Ready ReadyModel
    | Errored


editing : Model -> Track -> Model
editing model track =
    case model of
        Ready readyModel ->
            Ready { readyModel | uiMode = Editing track }

        _ ->
            model


type Msg
    = Loaded (Result Http.Error TrackList)
    | NoOp
    | OpenEditor Track
    | CloseEditor Track
    | EditTrack Track
    | SavedComment String (Result Http.Error ())
    | Tick Time.Posix
    | EnqueueSave Comment TrackId Posix
    | ChangeView UIMode


applyModel : (ReadyModel -> ReadyModel) -> Model -> Model
applyModel f model =
    case model of
        Ready readyModel ->
            Ready <| f readyModel

        _ ->
            model


replaceTrack : Track -> Model -> Model
replaceTrack track model =
    model
        |> applyModel (\m -> {m | tracks = List.map (\t -> if t.id == track.id then track else t) m.tracks})

changeTrack : Model -> Track -> Model
changeTrack model track =
    model
        |> applyModel (\m -> { m | uiMode = Editing track })


dequeue : Model -> TrackId -> Model
dequeue model trackId =
    model |> applyModel (\m -> { m | pendingCommands = Dict.remove trackId m.pendingCommands })


readyFromTracks : List Track -> ReadyModel
readyFromTracks tracks =
    { tracks = tracks
    , uiMode = Default
    , pendingCommands = Dict.empty
    }


timeAdd : Posix -> Int -> Posix
timeAdd posix millis =
    Time.millisToPosix <| Time.posixToMillis posix + millis


timeAfter : Posix -> Posix -> Bool
timeAfter pointOfReference other =
    Time.posixToMillis pointOfReference > Time.posixToMillis other


waitPeriod =
    2000


commandsFromQueue : (Posix -> Bool) -> ReadyModel -> List (Cmd Msg)
commandsFromQueue predicate { pendingCommands } =
    pendingCommands
        |> Dict.values
        |> List.filter (\c -> predicate c.time)
        |> List.map (\c -> postComment c.comment c.trackId)


enqueueReadyRequests : Model -> Posix -> ( Model, Cmd Msg )
enqueueReadyRequests model currentTime =
    let
        waitTimeOver =
            \time -> timeAfter currentTime <| timeAdd time waitPeriod
    in
    case model of
        Ready readyModel ->
            ( Ready { readyModel | pendingCommands = Dict.map (\_ v -> { v | started = waitTimeOver v.time }) readyModel.pendingCommands }
            , Cmd.batch (commandsFromQueue waitTimeOver readyModel)
            )

        _ ->
            ( model, Cmd.none )


emptyComment : Comment
emptyComment =
    { headline = ""
    , excerpt = ""
    }


enqueueCommentSave : Track -> Cmd Msg
enqueueCommentSave track =
    Task.perform (EnqueueSave (Maybe.withDefault emptyComment track.comment) track.id) Time.now


update msg model =
    case msg of
        Loaded (Ok tracks) ->
            ( Ready <| readyFromTracks tracks, Cmd.none )

        Loaded (_) -> (Errored, Cmd.none)

        OpenEditor track ->
            ( editing model track, Cmd.none )

        CloseEditor track ->
            ( model |> replaceTrack track |> applyModel (\m -> { m | uiMode = Default }), Cmd.none )

        ChangeView uiMode ->
            ( applyModel (\m -> { m | uiMode = uiMode }) model, Cmd.none )

        EditTrack track ->
            ( editing (changeTrack model track) track, enqueueCommentSave track )

        EnqueueSave comment trackId now ->
            ( applyModel
                (\m ->
                    { m
                        | pendingCommands =
                            Dict.insert
                                trackId
                                { time = now
                                , comment = comment
                                , trackId = trackId
                                , started = False
                                }
                                m.pendingCommands
                    }
                )
                model
            , Cmd.none
            )

        SavedComment trackId (Ok ()) ->
            ( dequeue model trackId, Cmd.none )

        SavedComment trackId (Err _) ->
            Debug.log "failedToSave" <| (dequeue model trackId, Cmd.none)

        Tick time ->
            enqueueReadyRequests model time

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

        Ready { tracks } ->
            List.map trackCard tracks

        Errored ->
            [Html.text "an error occurred"]


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
        Ready { tracks } ->
            if showGraph then
                [ graph "green" "energy" 1.0 <| List.map (\t -> t.features.energy) tracks
                , graph "red" "tempo" 200.0 <| List.map (\t -> t.features.tempo) tracks
                , graph "blue" "danceability" 1.0 <| List.map (\t -> t.features.danceability) tracks
                ]

            else
                []
        _ -> []


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
        , style "background-color" "rgba(0, 0, 0, 0.6)"
        , style "height" "100vh"
        , style "width" "100vw"
        , style "top" "0"
        , style "left" "0"
        ]
        [ section
            [ style "top" "40%"
            , style "width" "40%"
            , style "background-color" "rgba(255, 255, 255, 0.9)"
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
            [ input
                [ style "display" "block"
                , style "width" "100%"
                , style "margin-bottom" "1em"
                , onInput (setCommentHeadline track >> EditTrack)
                , value <| Maybe.withDefault "" <| Maybe.map .headline track.comment
                ]
                []
            , textarea
                [ style "display" "block"
                , style "width" "100%"
                , style "height" "8em"
                , style "resize" "none"
                , style "border" "none"
                , style "background-color" "rgba(0,0,0,.01)"
                , style "padding" "1em"
                , style "box-sizing" "border-box;"
                , onInput (setCommentExcerpt track >> EditTrack)
                , value <| Maybe.withDefault "" <| Maybe.map .excerpt track.comment
                ]
                []
            , button
                [ onClick (CloseEditor track) ]
                [ Html.text "Done" ]
            ]
        ]


modalContainer : Model -> Html Msg
modalContainer model =
    case model of
        Ready { uiMode } ->
            case uiMode of
                Editing track ->
                    editingView track

                _ ->
                    Html.text ""

        _ ->
            Html.text ""


view model =
    div [ class "py-5" ]
        [ div [ class "row" ] <| visualization model
        , div [] <| cards model
        , modalContainer model
        ]
