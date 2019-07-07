{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.SpotifyLogin where

import Import

getSpotifyLoginR :: Handler Html
getSpotifyLoginR =
    redirect ("https://accounts.spotify.com/authorize"::String)
