{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.AnnotatedTrackListSpec (spec) where

import TestImport
import Handler.AnnotatedTrackList
import Data.Aeson
import Data.ByteString.Lazy as L

spec :: Spec
spec = describe "valid json" $ do
    it "parses" $ do
        file <- liftIO (L.readFile "test/playlist-fixture.json")
        let parsed = decode file :: Maybe TrackList in
            parsed `shouldBe` (Just $ TrackList [])

