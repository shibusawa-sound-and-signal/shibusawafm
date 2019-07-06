{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SpotifyRest where

import Import
import Data.Aeson
import Network.HTTP.Req as R
import Control.Monad (mzero)

(|>) :: Functor f => f a -> (a -> b) -> f b
a |> b = fmap b a


data Artist = Artist {
    name :: Text,
    artistId :: Text
} deriving (Show, Eq)

instance FromJSON Artist where
     parseJSON (Object o) = Artist <$> (o .: "name")
                <*> (o .: "id")
     parseJSON _ = mzero

data Image = Image {
    url :: Text,
    height :: Int,
    width :: Int
} deriving (Show, Eq)

instance FromJSON Image where
     parseJSON (Object o) = Image <$> (o .: "url")
                <*> (o .: "height")
                <*> (o .: "width")
     parseJSON _ = mzero

data Album = Album {
    name :: Text,
    albumId :: Text,
    images :: [Image]
} deriving (Show, Eq)

instance FromJSON Album where
     parseJSON (Object o) = Album <$> (o .: "name")
                <*> (o .: "id")
                <*> (o .: "images")
     parseJSON _ = mzero

data Track = Track {
    trackId :: Text,
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

data AudioFeatures = AudioFeatures {
    audioFeaturesTrackId :: Text,
    danceability :: Float,
    energy :: Float,
    tempo :: Float
} deriving (Show, Eq)

data AudioFeaturesList = AudioFeaturesList {
    audioFeatures :: [AudioFeatures]
} deriving (Show, Eq)


instance FromJSON AudioFeatures where
 parseJSON (Object o) =
    AudioFeatures <$> (o .: "id")
           <*> (o .: "danceability")
           <*> (o .: "energy")
           <*> (o .: "tempo")
 parseJSON _ = mzero


instance FromJSON AudioFeaturesList where
 parseJSON (Object o) =
    AudioFeaturesList <$> (o .: "audio_features")
 parseJSON _ = mzero


data AudioAnalysis = AudioAnalysis {
    audioAnalysisTrackId :: Text
} deriving (Show, Eq)


instance FromJSON AudioAnalysis where
 parseJSON (Object o) =
    AudioAnalysis <$> (o .: "id")
 parseJSON _ = mzero

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
            "id" .= trackId,
            "title" .= title,
            "artists" .= artists,
            "album" .= album
        ]

instance ToJSON Artist where
    toJSON Artist {..} = object
        [
            "id" .= artistId,
            "name" .= name
        ]


instance ToJSON Image where
    toJSON Image {..} = object
        [
            "src" .= url,
            "height" .= height,
            "width" .= width
        ]

instance ToJSON Album where
    toJSON Album {..} = object
        [
            "id" .= albumId,
            "name" .= name,
            "images" .= images
        ]


instance FromJSON TrackList where
    parseJSON = withObject "TrackList" $ \o -> do
        tracks <- o .: "tracks"
        items <- tracks .: "items"
        return $ TrackList items

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

getAudioFeatures :: MonadHttp m => Text -> [Text] -> m AudioFeaturesList
getAudioFeatures accessToken trackIds =
    let url = (https "api.spotify.com"/:"v1"/:"audio-features")
        trackIdParamValue = Just $ intercalate "," trackIds
        options = header "Authorization" (concat ["Bearer ", encodeUtf8 accessToken]) <> queryParam "ids" trackIdParamValue in
            req R.GET url NoReqBody jsonResponse options |> R.responseBody

getAudioAnalysis :: MonadHttp m => Text -> Text -> m AudioAnalysis
getAudioAnalysis accessToken trackId =
    let url = (https "api.spotify.com"/:"v1"/:"audio-analysis" /: trackId)
        options = header "Authorization" (concat ["Bearer ", encodeUtf8 accessToken]) in
            req R.GET url NoReqBody jsonResponse options |> R.responseBody