{-# LANGUAGE OverloadedStrings #-}
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

getAnnotatedTrackListR :: Handler Value
getAnnotatedTrackListR = returnJson $ TrackAnnotation "Holy Cow"