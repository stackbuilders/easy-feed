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
    Feed (..)
  , Person (..)
  , TypeAttribute (..)
    -- * Feed rendering
  , renderFeed )
where

import Data.Aeson
import Data.Text (Text)
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
  , feedTitleType :: !(Maybe TypeAttribute)
    -- ^ The feed title type (Text, Html or XHtml)
  , feedAuthors :: ![Person]
    -- The feed authors
  } deriving (Eq, Ord, Show, Read)

instance ToJSON Feed where
  toJSON Feed {..} = object
    [ "title" .= feedTitle
    , "titleType" .= feedTitleType
    , "author" .= feedAuthors ]

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
