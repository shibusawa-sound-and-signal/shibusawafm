{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handler.AnnotatedTrackList where

import Import
import Network.HTTP.Req as R
import Data.Aeson
import Control.Monad (mzero)

(|>) :: Functor f => f a -> (a -> b) -> f b
a |> b = fmap b a

data Artist = Artist {
    name :: Text,
    id :: Text
} deriving (Show, Eq)

instance FromJSON Artist where
     parseJSON (Object o) = Artist <$> (o .: "name")
                <*> (o .: "id")
     parseJSON _ = mzero

data Album = Album {
    name :: Text,
    id :: Text
} deriving (Show, Eq)

instance FromJSON Album where
     parseJSON (Object o) = Album <$> (o .: "name")
                <*> (o .: "id")
     parseJSON _ = mzero

data Track = Track {
    id :: Text,
    title :: Text,
    album :: Album,
    artists :: [Artist]
} deriving (Show, Eq)

data TokenResponse = TokenResponse {
    accessToken :: Text,
    tokenType :: Text,
    expiresIn :: Int,
    scope :: Text
} deriving (Show, Eq)

instance FromJSON TokenResponse where
 parseJSON (Object o) =
    TokenResponse <$> (o .: "access_token")
           <*> (o .: "token_type")
           <*> (o .: "expires_in")
           <*> (o .: "scope")
 parseJSON _ = mzero

instance FromJSON Track where
 parseJSON (Object o) =
    Track <$> ((o .: "track") >>= (.: "id"))
           <*> ((o .: "track") >>= (.: "name"))
           <*> ((o .: "track") >>= (.: "album"))
           <*> ((o .: "track") >>= (.: "artists"))
 parseJSON _ = mzero

newtype TrackList = TrackList [Track] deriving (Show, Eq, ToJSON)

instance ToJSON Track where
    toJSON Track {..} = object
        [
            "id" .= id,
            "title" .= title,
            "artists" .= artists,
            "album" .= album
        ]

instance ToJSON Artist where
    toJSON Artist {..} = object
        [
            "id" .= id,
            "name" .= name
        ]

instance ToJSON Album where
    toJSON Album {..} = object
        [
            "id" .= id,
            "name" .= name
        ]


instance FromJSON TrackList where
    parseJSON = withObject "TrackList" $ \o -> do
        tracks <- o .: "tracks"
        items <- tracks .: "items"
        return $ TrackList items

data TrackAnnotation = TrackAnnotation
    { content :: Text
    }

instance ToJSON TrackAnnotation where
    toJSON TrackAnnotation {..} = object
        [ "content" .= content
        ]

getAccessToken :: MonadHttp m => Text -> Text -> m TokenResponse
getAccessToken secret refreshToken =
    let url = https "accounts.spotify.com" /: "api" /: "token"
        options = header "Authorization" (concat ["Basic ", encodeUtf8 secret]) <>
                    header "Content-Type" "application/x-www-form-urlencoded"
        body = R.ReqBodyUrlEnc ("grant_type" =: ("refresh_token" :: Text) <> "refresh_token" =: refreshToken) in do
            req R.POST url body jsonResponse options |> R.responseBody

getTrackList :: MonadHttp m => Text -> Text -> m TrackList
getTrackList accessToken playlistId =
    let url = (https "api.spotify.com"/:"v1"/:"playlists" /: playlistId)
        options = header "Authorization" (concat ["Bearer ", encodeUtf8 accessToken]) in
            req R.GET url NoReqBody jsonResponse options |> R.responseBody

getAnnotatedTrackListR :: Text -> Handler Value
getAnnotatedTrackListR playlistOrAlbumId = do
    App {..} <- getYesod
    let AppSettings {..} = appSettings in
        R.runReq def $ do
            TokenResponse {..} <- getAccessToken spotifySecret spotifyKey
            tracks <- getTrackList accessToken playlistOrAlbumId
            returnJson tracks