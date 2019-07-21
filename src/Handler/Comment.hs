{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.Comment where

import Import

data CommentCreateRequest = CommentCreateRequest
    {
        headline:: Text,
        excerpt:: Text
    }

instance FromJSON CommentCreateRequest where
    parseJSON (Object o) =
        CommentCreateRequest <$> (o .: "headline")
            <*> (o .: "excerpt")
    parseJSON _ = mzero

commentFromRequest :: Text -> CommentCreateRequest -> TrackComment
commentFromRequest spotifyTrackId request =
    TrackComment {
        trackCommentSpotifyId = spotifyTrackId,
        trackCommentHeadline = headline request,
        trackCommentExcerpt = excerpt request
    }

postCommentR :: Text -> Handler Value
postCommentR trackId = do
    commentCreateRequest <- (requireCheckJsonBody :: Handler CommentCreateRequest)
    existingCommentId <- runDB $ getBy $ UniqueSpotifyId trackId

    result <- case existingCommentId of
        Just (Entity trackCommentPrimaryKey _) ->
            runDB $ replace trackCommentPrimaryKey $ commentFromRequest trackId commentCreateRequest
        Nothing -> do
            _ <- runDB $ insert $ commentFromRequest trackId commentCreateRequest
            return ()

    returnJson result
