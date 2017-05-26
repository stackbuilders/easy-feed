{-# LANGUAGE OverloadedStrings #-}

module Text.Feed.RSSSpec
  ( spec )
where

import Data.These
import Test.Hspec

import qualified Text.Feed.RSS as RSS

import Debug.Trace

spec :: Spec
spec = do
  describe "RSS feed" $ do
    it "creates a RSS feed" $ do
      let rssFeed = testingRSSFeed
        
      traceShowId(RSS.renderFeed rssFeed) == "hola"
      

testingRSSItem :: RSS.Item
testingRSSItem =
  RSS.Item
    { RSS.itemTitleDesc = These "Item Title" "Item Description"
    , RSS.itemLink = Just "Item Link"
    , RSS.itemAuthor = Just "Item Author"
    , RSS.itemCategory = ["Item Category1", "Item Category2"]
    , RSS.itemComments = Just "Item Comments"
    , RSS.itemGuid = Just "Item Guid"
    , RSS.itemPubDate = Nothing
    }

testingRSSFeed :: RSS.Feed
testingRSSFeed =
  RSS.Feed
    { RSS.feedTitle = "Test Feed Title"
    , RSS.feedLink = "myfeed@link"
    , RSS.feedDescription = "Feed Description"
    , RSS.feedLanguage = Just RSS.English
    , RSS.feedCopyright = Just "Feed CC"
    , RSS.feedManagingEditor = Just "Feed Managing Editor"
    , RSS.feedWebMaster = Just "Feed Webmaster"
    , RSS.feedPubDate = Nothing
    , RSS.feedLastBuildDate = Nothing
    , RSS.feedCategory = ["Some category"]
    , RSS.feedDocs = Just "Feed Docs"
    , RSS.feedTimeToLive = Just 60
    , RSS.feedImage = Nothing
    , RSS.feedItems = [testingRSSItem]
    }