{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.AnnotatedTrackList where

import Import

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