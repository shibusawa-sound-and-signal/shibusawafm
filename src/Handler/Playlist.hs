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
        comment :: Maybe TrackComment
    }

instance ToJSON CommentedTrack where
    toJSON CommentedTrack {..} = object
        [
            "id" .= commentedTrackId,
            "title" .= title,
            "artists" .= artists,
            "album" .= album,
            "comment" .= comment
        ]

getTracks :: TrackList -> [Track]
getTracks (TrackList ts) = ts

selectCommentsBySpotifyId :: [Text] -> DB [Entity TrackComment]
selectCommentsBySpotifyId spotifyTrackIds = selectList [TrackCommentSpotifyId <-. spotifyTrackIds] [Asc TrackCommentId]


makeCommentedTrack :: Track -> Maybe TrackComment -> CommentedTrack
makeCommentedTrack Track {..} maybeComment =
    CommentedTrack {
        commentedTrackId = trackId,
        title= title,
        artists= artists,
        album= album,
        comment = maybeComment
    }

findComment :: [TrackComment] -> Text -> Maybe TrackComment
findComment comments spotifyTrackId =
    find (\comment -> (trackCommentSpotifyId comment) == spotifyTrackId) comments

mergeCommentsAndTracks :: TrackList -> [TrackComment] -> [CommentedTrack]
mergeCommentsAndTracks (TrackList tracks) comments =
    map
        (\track -> makeCommentedTrack track (findComment comments (trackId track)))
        tracks

valueFromEntity :: Entity a -> a
valueFromEntity (Entity _ value) = value

getPlaylistR :: Text -> Handler Value
getPlaylistR playlistOrAlbumId = do
    App {..} <- getYesod
    let AppSettings {..} = appSettings in do
        trackList <- R.runReq def $ do
            TokenResponse {..} <- getAccessToken spotifySecret spotifyKey
            tracks <- getTrackList accessToken playlistOrAlbumId
            pure tracks
        comments <- runDB $ selectCommentsBySpotifyId $ map trackId $ getTracks trackList
        returnJson $ mergeCommentsAndTracks trackList $ map valueFromEntity comments