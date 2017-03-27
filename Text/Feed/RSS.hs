-- |
-- Module      :  Text.Feed.RSS
-- Copyright   :  © 2017 Stack Builders
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <mkarpov@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Everything you need for RSS feed generation.
--
-- This module is intended to be imported qualified:
--
-- > import qualified Text.Feed.RSS

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.Feed.RSS
  ( -- * Types
    Feed (..)
  , Item (..)
  , Image (..)
    -- * Feed rendering
  , renderFeed )
where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Lazy as BL

-- | A representation of a RSS feed.
--
-- See also: <http://www.rssboard.org/rss-specification>.

data Feed = Feed
  { feedTitle :: !Text
    -- ^ The name of the channel
  , feedLink :: !Text
    -- ^ The URL to the HTML website
  , feedDescription :: !Text
    -- ^ Phrase or sentence describing the channel
  , feedLanguage :: !(Maybe Language)
    -- ^ The language the channel is written in
  , feedCopyright :: !(Maybe Text)
    -- ^ Copyright notice for content in the channel
  , feedManagingEditor :: !(Maybe Text)
    -- ^ Email address for person responsible of editorial content
  , feedWebMaster :: !(Maybe Text)
    -- ^ Email address for person responsible of technical issues relating
    -- to the channel
  , feedPubDate :: !(Maybe UTCTime)
    -- ^ Publication date for the content
  , feedLastBuildDate :: !(Maybe UTCTime)
    -- ^ Last time the content changed
  , feedCategory :: ![Text]
    -- ^ One or more categories that the channel belongs to
  , feedDocs :: !(Maybe Text)
    -- ^ A URL pointing to the documentation for the format used
  , feedTimeToLive :: !(Maybe Int)
    -- ^ Number of minutes that indicates how long a channel can be cached
    -- before refreshing from the source
  , feedImage :: !(Maybe Image)
    -- ^ Image to be displayed with channel
  , feedItems :: ![Item]
    -- ^ Collection of feed items
  } deriving (Eq, Ord, Show, Read)

-- | An RSS item

data Item = Item
  {
  } deriving (Eq, Ord, Show, Read)

-- | Information about image to be displayed with channel.

data Image = Image
  { imageUrl   :: !Text
    -- ^ The URL of a GIF, JPEG or PNG image that represents the channel
  , imageTitle :: !Text
    -- ^ Description of the image, will be used as @alt@ attribute when the
    -- image is rendered in HTML
  , imageLink  :: !Text
    -- ^ URL of the site, when the image
  , imageWidth :: !(Maybe Int)
    -- ^ Optional width: maximum value is 144, default value is 88
  , imageHeigh :: !(Maybe Int)
    -- ^ Optional height: maximum height is 400, default value is 31
  } deriving (Eq, Ord, Show, Read)

-- | Enumeration of languages.
--
-- See also: <http://www.rssboard.org/rss-language-codes>.

data Language
  = Africans
  | Albanian
  | Basque
  | Belarusian
  | Bulgarian
  | Catalan
  | ChineseSimplified
  | ChineseTraditional
  | Croatian
  | Czech

  | English
  | Russian
  | Spanish
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- | Render a 'RSSFeed' as a lazy 'BL.ByteString'.

renderFeed :: Feed -> BL.ByteString
renderFeed = undefined

----------------------------------------------------------------------------
-- Helpers

-- | Get identifier for a 'Language'.

getLangId :: Language -> Text
getLangId Africans           = "af"
getLangId Albanian           = "sq"
getLangId Basque             = "eu"
getLangId Bulgarian          = "bg"
getLangId Catalan            = "ca"
getLangId ChineseSimplified  = "zh-cn"
getLangId ChineseTraditional = "zh-tw"
getLangId Croatian           = "hr"
getLangId Czech              = "cs"

getLangId English            = "en"
getLangId Russian            = "ru"
getLangId Spanish            = "es"
