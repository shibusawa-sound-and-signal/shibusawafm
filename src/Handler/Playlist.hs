{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Playlist where

import Import
import SpotifyRest
import Network.HTTP.Req as R

data CommentedTrack = CommentedTrack
    {
        commentedTrackId :: Text,
        title :: Text,
        album :: Album,
        artists :: [Artist],
        danceability :: Maybe Float,
        energy ::  Maybe Float,
        tempo :: Maybe Float,
        comment :: Maybe TrackComment
    }

instance ToJSON CommentedTrack where
    toJSON CommentedTrack {..} = object
        [
            "id" .= commentedTrackId,
            "title" .= title,
            "artists" .= artists,
            "album" .= album,
            "danceability" .= danceability,
            "energy" .= energy,
            "tempo" .= tempo,
            "comment" .= comment
        ]

getTracks :: TrackList -> [Track]
getTracks (TrackList ts) = ts

selectCommentsBySpotifyId :: [Text] -> DB [Entity TrackComment]
selectCommentsBySpotifyId spotifyTrackIds = selectList [TrackCommentSpotifyId <-. spotifyTrackIds] [Asc TrackCommentId]


makeCommentedTrack :: Track -> Maybe TrackComment -> Maybe AudioFeatures -> CommentedTrack
makeCommentedTrack Track {..} maybeComment maybeFeatures =
    CommentedTrack {
        commentedTrackId = trackId,
        title= title,
        artists= artists,
        album= album,
        comment = maybeComment,
        danceability = map SpotifyRest.danceability maybeFeatures,
        energy = map SpotifyRest.energy maybeFeatures,
        tempo = map SpotifyRest.tempo maybeFeatures
    }

findComment :: [TrackComment] -> Text -> Maybe TrackComment
findComment comments spotifyTrackId =
    find (\comment -> (trackCommentSpotifyId comment) == spotifyTrackId) comments

findFeatures :: [AudioFeatures] -> Text -> Maybe AudioFeatures
findFeatures features spotifyTrackId =
    find (\feature -> (audioFeaturesTrackId feature) == spotifyTrackId) features

findAnalysis :: [AudioAnalysis] -> Text -> Maybe AudioAnalysis
findAnalysis analyses spotifyTrackId =
    find (\analysis -> (audioAnalysisTrackId analysis) == spotifyTrackId) analyses

mergeCommentsAndTracks :: TrackList -> [TrackComment] -> [AudioFeatures] -> [CommentedTrack]
mergeCommentsAndTracks (TrackList tracks) comments features =
    map
        (\track -> makeCommentedTrack track
            (findComment comments (trackId track))
            (findFeatures features (trackId track)))
        tracks

valueFromEntity :: Entity a -> a
valueFromEntity (Entity _ value) = value

readonlyToken :: HandlerFor App Text
readonlyToken = do
    App {..} <- getYesod
    let AppSettings {..} = appSettings in do
        token <- R.runReq def $ do
            TokenResponse {..} <- getAccessToken spotifySecret spotifyKey
            pure accessToken
        return token

accessTokenFromContext :: HandlerFor App Text
accessTokenFromContext = do
    currentUser <- maybeAuth
    token <- case currentUser of
        Just (Entity _ user) -> return $ userToken user
        Nothing -> readonlyToken
    return token

getPlaylistsForCurrentUserR :: Handler Value
getPlaylistsForCurrentUserR = do
    accessToken <- accessTokenFromContext
    items <- R.runReq def $ getMyPlaylists accessToken
    returnJson $ items


getPlaylistR :: Text -> Handler Value
getPlaylistR playlistOrAlbumId = do
    accessToken <- accessTokenFromContext
    (trackList, features) <- R.runReq def $ do
        tracks <- getTrackList accessToken playlistOrAlbumId
        audioFeatures <- getAudioFeatures accessToken $ map trackId $ getTracks tracks
--         audioAnalyses <- mapM (getAudioAnalysis accessToken) (map trackId $ getTracks tracks)
        pure (tracks, audioFeatures)

    comments <- runDB $ selectCommentsBySpotifyId $ map trackId $ getTracks trackList
    returnJson $ mergeCommentsAndTracks trackList (map valueFromEntity comments) (audioFeatures features)