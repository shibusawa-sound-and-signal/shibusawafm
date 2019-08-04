{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes #-}

module AccessToken where

import Import
import SpotifyRest
import Network.HTTP.Req as R

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
