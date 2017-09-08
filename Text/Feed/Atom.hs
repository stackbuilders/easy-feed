-- |
-- Module      :  Text.Feed.Atom
-- Copyright   :  © 2017 Stack Builders
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <mkarpov@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Everything you need for Atom feed generation.
--
-- This module is intended to be imported qualified:
--
-- > import qualified Text.Feed.Atom as Atom

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.Feed.Atom
  ( -- * Types
    Category (..)
  , Feed (..)
  , Generator (..)
  , Icon (..)
  , Link (..)
  , Logo (..)
  , Person (..)
  , TypeAttribute (..)
    -- * Feed rendering
  , renderFeed )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Text.Mustache
import qualified Data.Text.Lazy as TL

#if MIN_VERSION_template_haskell(2,11,0)
import qualified Text.Mustache.Compile.TH as TH
#else
import Data.FileEmbed (embedStringFile)
import Text.Megaparsec (parseErrorPretty)
#endif

-- | A representation of a Atom feed.
--
-- See also: <https://www.ietf.org/rfc/rfc4287.txt>.

data Feed = Feed
  { feedTitle :: !Text
    -- ^ The name of the channel
  , feedAuthors :: ![Person]
    -- ^ The feed authors
  , feedCategories :: ![Category]
    -- ^ The list of categories
  , feedContributors :: ![Person]
    -- ^ The feed contributors (a Person construct like the Authors)
  , feedGenerator :: !(Maybe Generator)
    -- ^ The feed generator
  , feedIcon :: !(Maybe Icon)
    -- ^ The feed icon URI
  , feedId :: !Text
    -- ^ The feed identifier
  , feedLinks :: ![Link]
    -- ^ The feed links
  , feedLogo :: !(Maybe Logo)
    -- ^ The feed logo
  , feedRights :: !(Maybe Text)
    -- ^ The feed rights
  , feedSubtitle :: !(Maybe Text)
    -- ^ The feed subtitle
  , feedUpdated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)

instance ToJSON Feed where
  toJSON Feed {..} = object
    [ "title" .= feedTitle
    , "author" .= feedAuthors
    , "category" .= feedCategories
    , "contributor" .= feedContributors
    , "generator" .= feedGenerator
    , "icon" .= feedIcon
    , "id" .= feedId
    , "link" .= feedLinks
    , "logo" .= feedLogo
    , "rights" .= feedRights
    , "subtitle" .= feedSubtitle
    , "updated" .= (formatFeedDate <$> feedUpdated) ]

-- | An enumeration for the Type attribute on Text constructs

data TypeAttribute =
    TextType
  | HtmlType
  | XHtmlType
  deriving (Eq, Ord, Show, Read)

instance ToJSON TypeAttribute where
  toJSON = String . getTypeAttributeText

-- | Information about a feed or atom author

data Person = Person
  { personName  :: !Text
  , personEmail :: !(Maybe Text)
  , personUri   :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Read)

instance ToJSON Person where
  toJSON Person {..} = object
    [ "name"  .= personName
    , "email" .= personEmail
    , "uri"   .= personUri
    ]

-- | The category of the element

data Category = Category
  { categoryTerm   :: !Text
  , categoryScheme :: !(Maybe Text)
  , categoryLabel  :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Read)

instance ToJSON Category where
  toJSON Category {..} = object
    [ "term"   .= categoryTerm
    , "scheme" .= categoryScheme
    , "label"  .= categoryLabel
    ]

-- | The generator of the Feed or Atom
data Generator = Generator
  { generatorUri     :: !(Maybe Text)
  , generatorVersion :: !(Maybe Text)
  , generatorText    :: !Text
  } deriving (Eq, Ord, Show, Read)

instance ToJSON Generator where
  toJSON Generator {..} = object
    [ "uri"     .= generatorUri
    , "version" .= generatorVersion
    , "text"    .= generatorText
    ]

-- | The icon uri
newtype Icon = Icon
  { unIcon :: Text }
  deriving(Eq, Ord, Show, Read)

instance ToJSON Icon where
  toJSON (Icon uri) = String uri

-- | The feed link reference
data Link = Link
  { linkHref     :: !Text
  , linkRel      :: !(Maybe Text)
  , linkType     :: !(Maybe Text)
  , linkHrefLang :: !(Maybe Text)
  , linkTitle    :: !(Maybe Text)
  , linkLength   :: !(Maybe Text)
  } deriving(Eq, Ord, Show, Read)

instance ToJSON Link where
  toJSON Link {..} = object
    [ "href"     .= linkHref
    , "rel"      .= linkRel
    , "type"     .= linkType
    , "hreflang" .= linkHrefLang
    , "title"    .= linkTitle
    , "length"   .= linkLength
    ]

-- | The Logo element
newtype Logo = Logo
  { unLogo :: Text }
  deriving(Eq, Ord, Show, Read)

instance ToJSON Logo where
  toJSON (Logo uri) = String uri

-- -- | A date construct with optional locale
-- data DateConstruct = DateConstruct
--   { dateTime :: !UTCTime
--   , dateLocale :: !(Maybe TimeLocale)
--   } deriving(Eq, Ord, Show)

-- instance ToJSON DateConstruct where
--   toJSON DateConstruct {..} =
--     String $ pack $ formatFeedDate dateLocale dateTime

-- | Render a 'Feed' as a lazy 'TL.Text'.

renderFeed :: Feed -> TL.Text
renderFeed =
#if MIN_VERSION_template_haskell(2,11,0)
  renderMustache $(TH.compileMustacheFile "templates/atom.mustache") . toJSON
#else
  case compileMustacheText "main" $(embedStringFile "templates/atom.mustache") of
    Left err -> error (parseErrorPretty err)
    Right template -> renderMustache template . toJSON
#endif

----------------------------------------------------------------------------
-- Helpers

-- | Get the text to render for the type attribute

getTypeAttributeText :: TypeAttribute -> Text
getTypeAttributeText = \case
  TextType  -> "text"
  HtmlType  -> "html"
  XHtmlType -> "xhtml"

formatFeedDate :: UTCTime -> String
formatFeedDate =
  formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q"))
