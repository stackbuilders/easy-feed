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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.Feed.RSS
  ( RSSFeed (..)
  , RSSItem (..)
  , renderRSSFeed )
where

import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL

-- | A representation of a RSS feed.
--
-- See also: <http://www.rssboard.org/rss-specification>.

data RSSFeed = RSSFeed
  { rssFeedTitle :: !Text
    -- ^ The name of the channel
  , rssFeedLink :: !Text
    -- ^ The URL to the HTML website
  , rssFeedDescription :: !Text
    -- ^ Phrase or sentence describing the channel
  , rssFeedLanguage :: !(Maybe Language)
    -- ^ The language the channel is written in
  , rssFeedItems :: ![RSSItem]
    -- ^ Collection of feed items
  } deriving (Eq, Ord, Show, Read)

-- |

data RSSItem = RSSItem
  {
  } deriving (Eq, Ord, Show, Read)

-- | Enumeration of languages.
--
-- See also: <http://www.rssboard.org/rss-language-codes>.

data Language
  = English
  | Russian
  | Spanish
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- | Render a 'RSSFeed' as a lazy 'BL.ByteString'.

renderRSSFeed :: RSSFeed -> BL.ByteString
renderRSSFeed = undefined

----------------------------------------------------------------------------
-- Helpers

-- | Get identifier for a 'Language'.

getLangId :: Language -> Text
getLangId English = "en"
getLangId Russian = "ru"
getLangId Spanish = "es"
