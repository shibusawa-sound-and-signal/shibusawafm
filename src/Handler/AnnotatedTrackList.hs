{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.AnnotatedTrackList where

import Import
import Network.HTTP.Req
import Data.Aeson
import Data.Monoid
import Control.Monad (mzero)

data Artist = Artist {
    name :: Text
} deriving (Show, Eq)

instance FromJSON Artist where
     parseJSON (Object o) = Artist <$> ((o .: "name"))
     parseJSON _ = mzero

data Track = Track {
    id :: Text,
    title :: Text,
    artists :: [Artist]
} deriving (Show, Eq)

instance FromJSON Track where
 parseJSON (Object o) =
    Track <$> ((o .: "track") >>= (.: "id"))
           <*> ((o .: "track") >>= (.: "name"))
           <*> ((o .: "track") >>= (.: "artists"))
 parseJSON _ = mzero

newtype TrackList = TrackList [Track] deriving (Show, Eq)

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

getAnnotatedTrackListR :: Text -> Handler Value
getAnnotatedTrackListR playlistOrAlbumId = do
    App {..} <- getYesod
    let AppSettings {..} = appSettings
       in returnJson $ TrackAnnotation $ "Holy Cow " ++ playlistOrAlbumId ++ spotifyKey