{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes #-}

module Handler.UserProfile where

import Import
import AccessToken
import SpotifyRest
import Network.HTTP.Req as R

data UserProfileResponse = UserProfileResponse {
  ownedPlaylists :: [PlaylistSummary]
}  deriving (Show, Eq)

instance ToJSON UserProfileResponse where
  toJSON UserProfileResponse {..} = object
      [
          "ownedPlaylists" .= ownedPlaylists
      ]


getUserProfileApiR :: Handler Value
getUserProfileApiR = do
    accessToken <- accessTokenFromContext
    Entity _ user <- requireAuth
    MyPlaylistsResponse {..} <- R.runReq def $ getMyPlaylists accessToken
    returnJson $ UserProfileResponse { ownedPlaylists = filter (\item -> (playlistOwnerId item) == userIdent user) myPlaylistsResponseItems }