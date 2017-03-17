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

import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL

-- | A representation of a RSS feed.
--
-- See also: <http://www.rssboard.org/rss-specification>.

data RSSFeed = RSSFeed
  { rssFeedTitle :: !Text
    -- ^ The name of the channel
  , rssFeedLink :: !URL
    -- ^ The URL to the HTML website
  , rssFeedDescription :: !Text
    -- ^ Phrase or sentence describing the channel
  , rssFeedLanguage :: !(Maybe Language)
    -- ^ The language the channel is written in
  , rssCopyright :: !(Maybe Text)
    -- ^ Copyright notice for content in the channel
  , rssManagingEditor :: !(Maybe Email)
    -- ^ Email for responsible of editorial content
  , rssWebMaster :: !(Maybe Email)
    -- ^ Email for resposible of technical issues
  , rssPubDate :: !(Maybe UTCTime)
    -- ^ Publication date for the content
  , rssLastBuildDate :: !(Maybe UTCTime)
    -- ^ Last time the content changed
  , rssCategory :: !(Maybe [Text])
    -- ^ One or more for categories that the channel belongs to
  , rssGenerator :: !(Maybe Text)
    -- ^ A string indicating the program used to generate channel
  , rssDocs :: !(Maybe URL)
    -- ^ A URL pointing to the documentation for the format used
  , rssCloud :: !(Maybe Cloud)
    -- ^ Allows processes to register with a cloud
  , rssTTL :: !(Maybe Integer)
    -- ^ Number of mintes for how long a channel can be cached
  , rssImage :: !(Maybe )
  , rssFeedItems :: ![RSSItem]
    -- ^ Collection of feed items
  } deriving (Eq, Ord, Show, Read)

-- |

data RSSItem = RSSItem
  {
  } deriving (Eq, Ord, Show, Read)

type Email = Text
type URL   = Text

data Cloud = Cloud
  { cloudDomain   :: !Text
  , cloudPort     :: !Text
  , cloudPath     :: !Text
  , cloudProtocol :: !Text
  , cloudRegisterProcedure :: !Text
  }

data Image = Image
  { imageUrl :: !URL
  , imageTitle :: !Text
  , imageLink  :: !URL
  , image
  }

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
