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
-- > import qualified Text.Feed.RSS as RSS

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.Feed.RSS
  ( -- * Types
    Feed (..)
  , mkFeed
  , Item (..)
  , mkItem
  , Image (..)
    -- * Feed rendering
  , renderFeed )
where

import Data.Aeson
import Data.Text (Text)
import Data.These
import Data.Time
import Text.Mustache
import qualified Data.Text.Lazy as TL

#if MIN_VERSION_template_haskell(2,11,0)
import qualified Text.Mustache.Compile.TH as TH
#else
import Data.FileEmbed (embedStringFile)
import Text.Megaparsec (parseErrorPretty)
#endif

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

-- | The helper takes only required feed parameters: title, link, and
-- description and sets everything else to 'Nothing' and @[]@.

mkFeed
  :: Text              -- ^ Title
  -> Text              -- ^ Link
  -> Text              -- ^ Description
  -> Feed
mkFeed title link desc = Feed
  { feedTitle          = title
  , feedLink           = link
  , feedDescription    = desc
  , feedLanguage       = Nothing
  , feedCopyright      = Nothing
  , feedManagingEditor = Nothing
  , feedWebMaster      = Nothing
  , feedPubDate        = Nothing
  , feedLastBuildDate  = Nothing
  , feedCategory       = []
  , feedDocs           = Nothing
  , feedTimeToLive     = Nothing
  , feedImage          = Nothing
  , feedItems          = [] }

instance ToJSON Feed where
  toJSON Feed {..} = object
    [ "title"          .= feedTitle
    , "link"           .= feedLink
    , "description"    .= feedDescription
    , "language"       .= feedLanguage
    , "copyright"      .= feedCopyright
    , "managingEditor" .= feedManagingEditor
    , "webMaster"      .= feedWebMaster
    , "pubDate"        .= (formatFeedDate <$> feedPubDate)
    , "lastBuildDate"  .= (formatFeedDate <$> feedLastBuildDate)
    , "category"       .= feedCategory
    , "docs"           .= feedDocs
    , "timeToLive"     .= feedTimeToLive
    , "image"          .= feedImage
    , "items"          .= feedItems ]

-- | An RSS item.

data Item = Item
  { itemTitleDesc :: !(These Text Text)
    -- ^ Title, description, or both of them
  , itemLink :: !(Maybe Text)
    -- ^ The URL of the item
  , itemAuthor :: !(Maybe Text)
    -- ^ Email address of the author of the item
  , itemCategory :: ![Text]
    -- ^ Collection of categories the item belongs to
  , itemComments :: !(Maybe Text)
    -- ^ URL of a page for comments relating to the item
  , itemGuid :: !(Maybe Text)
    -- ^ A string that uniquely identifies the item
  , itemPubDate :: !(Maybe UTCTime)
    -- ^ Date indicating when the item was published
  } deriving (Eq, Ord, Show, Read)

-- | The helper takes only required parameters: title or description or
-- both and create an 'Item'.

mkItem :: These Text Text -> Item
mkItem titleDesc = Item
  { itemTitleDesc = titleDesc
  , itemLink      = Nothing
  , itemAuthor    = Nothing
  , itemCategory  = []
  , itemComments  = Nothing
  , itemGuid      = Nothing
  , itemPubDate   = Nothing }

instance ToJSON Item where
  toJSON Item {..} = object
    [ "title"    .= itemTitle
    , "desc"     .= itemDesc
    , "author"   .= itemAuthor
    , "category" .= itemCategory
    , "comments" .= itemComments
    , "guid"     .= itemGuid
    , "pubDate"  .= (formatFeedDate <$> itemPubDate) ]
    where
      (itemTitle, itemDesc) =
        case itemTitleDesc of
          This title -> (Just title, Nothing)
          That desc  -> (Nothing, Just desc)
          These title desc -> (Just title, Just desc)

-- | Information about image to be displayed with channel.

data Image = Image
  { imageUrl :: !Text
    -- ^ The URL of a GIF, JPEG or PNG image that represents the channel
  , imageTitle :: !Text
    -- ^ Description of the image, will be used as @alt@ attribute when the
    -- image is rendered in HTML
  , imageLink :: !Text
    -- ^ URL of the site, when the image
  , imageWidth :: !(Maybe Int)
    -- ^ Optional width: maximum value is 144, default value is 88
  , imageHeight :: !(Maybe Int)
    -- ^ Optional height: maximum height is 400, default value is 31
  } deriving (Eq, Ord, Show, Read)

instance ToJSON Image where
  toJSON Image {..} = object
    [ "url"    .= imageUrl
    , "title"  .= imageTitle
    , "link"   .= imageLink
    , "width"  .= imageWidth
    , "height" .= imageHeight ]

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
  | Danish
  | Dutch
  | DutchBelgium
  | DutchNetherlands
  | English
  | EnglishAustralia
  | EnglishBelize
  | EnglishCanada
  | EnglishIreland
  | EnglishJamaica
  | EnglishNewZealand
  | EnglishPhillipines
  | EnglishSouthAfrica
  | EnglishTrinidad
  | EnglishUnitedKingdom
  | EnglishUnitedStates
  | EnglishZimbabwe
  | Estonian
  | Faeroese
  | Finnish
  | French
  | FrenchBelgium
  | FrenchCanada
  | FrenchFrance
  | FrenchLuxembourg
  | FrenchMonaco
  | FrenchSwitzerland
  | Galician
  | Gaelic
  | German
  | GermanAustria
  | GermanGermany
  | GermanLiechtenstein
  | GermanLuxembourg
  | GermanSwitzerland
  | Greek
  | Hawaiian
  | Hungarian
  | Icelandic
  | Indonesian
  | Irish
  | Italian
  | ItalianItaly
  | ItalianSwitzerland
  | Japanese
  | Korean
  | Macedonian
  | Norwegian
  | Polish
  | Portuguese
  | PortugueseBrazil
  | PortugeesePortugal
  | Romanian
  | RomanianMoldova
  | RomanianRomania
  | Russian
  | RussianMoldova
  | RussianRussia
  | Serbian
  | Slovak
  | Slovenian
  | Spanish
  | SpanishArgentina
  | SpanishBolivia
  | SpanishChile
  | SpanishColombia
  | SpanishCostaRica
  | SpanishDominicanRepublic
  | SpanishEcuador
  | SpanishElSalvador
  | SpanishGuatemala
  | SpanishHonduras
  | SpanishMexico
  | SpanishNicaragua
  | SpanishPanama
  | SpanishParaguay
  | SpanishPeru
  | SpanishPuertoRico
  | SpanishSpain
  | SpanishUruguay
  | SpanishVenezuela
  | Swedish
  | SwedishFinland
  | SwedishSweden
  | Turkish
  | Ukrainian
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance ToJSON Language where
  toJSON = String . getLangId

-- | Render a 'Feed' as a lazy 'TL.Text'.

renderFeed :: Feed -> TL.Text
renderFeed =
#if MIN_VERSION_template_haskell(2,11,0)
  renderMustache $(TH.compileMustacheFile "templates/rss.mustache") . toJSON
#else
  case compileMustacheText "main" $(embedStringFile "templates/rss.mustache") of
    Left err -> error (parseErrorPretty err)
    Right template -> renderMustache template . toJSON
#endif

----------------------------------------------------------------------------
-- Helpers

-- | Get identifier for a 'Language'.

getLangId :: Language -> Text
getLangId = \case
  Africans           -> "af"
  Albanian           -> "sq"
  Basque             -> "eu"
  Belarusian         -> "be"
  Bulgarian          -> "bg"
  Catalan            -> "ca"
  ChineseSimplified  -> "zh-cn"
  ChineseTraditional -> "zh-tw"
  Croatian           -> "hr"
  Czech              -> "cs"
  Danish             -> "da"
  Dutch              -> "nl"
  DutchBelgium       -> "nl-be"
  DutchNetherlands   -> "nl-nl"
  English            -> "en"
  EnglishAustralia   -> "en-au"
  EnglishBelize      -> "en-bz"
  EnglishCanada      -> "en-ca"
  EnglishIreland     -> "en-ie"
  EnglishJamaica     -> "en-jm"
  EnglishNewZealand  -> "en-nz"
  EnglishPhillipines -> "en-ph"
  EnglishSouthAfrica -> "en-za"
  EnglishTrinidad    -> "en-tt"
  EnglishUnitedKingdom -> "en-gb"
  EnglishUnitedStates -> "en-us"
  EnglishZimbabwe    -> "en-zw"
  Estonian           -> "et"
  Faeroese           -> "fo"
  Finnish            -> "fi"
  French             -> "fr"
  FrenchBelgium      -> "fr-be"
  FrenchCanada       -> "fr-ca"
  FrenchFrance       -> "fr-fr"
  FrenchLuxembourg   -> "fr-lu"
  FrenchMonaco       -> "fr-mc"
  FrenchSwitzerland  -> "fr-ch"
  Galician           -> "gl"
  Gaelic             -> "gd"
  German             -> "de"
  GermanAustria      -> "de-at"
  GermanGermany      -> "de-de"
  GermanLiechtenstein -> "de-li"
  GermanLuxembourg   -> "de-lu"
  GermanSwitzerland  -> "de-ch"
  Greek              -> "el"
  Hawaiian           -> "haw"
  Hungarian          -> "hu"
  Icelandic          -> "is"
  Indonesian         -> "in"
  Irish              -> "ga"
  Italian            -> "it"
  ItalianItaly       -> "it-it"
  ItalianSwitzerland -> "it-ch"
  Japanese           -> "ja"
  Korean             -> "ko"
  Macedonian         -> "mk"
  Norwegian          -> "no"
  Polish             -> "pl"
  Portuguese         -> "pt"
  PortugueseBrazil   -> "pt-br"
  PortugeesePortugal -> "pt-pt"
  Romanian           -> "ro"
  RomanianMoldova    -> "ro-mo"
  RomanianRomania    -> "ro-ro"
  Russian            -> "ru"
  RussianMoldova     -> "ru-mo"
  RussianRussia      -> "ru-ru"
  Serbian            -> "sr"
  Slovak             -> "sk"
  Slovenian          -> "sl"
  Spanish            -> "es"
  SpanishArgentina   -> "es-ar"
  SpanishBolivia     -> "es-bo"
  SpanishChile       -> "es-cl"
  SpanishColombia    -> "es-co"
  SpanishCostaRica   -> "es-cr"
  SpanishDominicanRepublic -> "es-do"
  SpanishEcuador     -> "es-ec"
  SpanishElSalvador  -> "es-sv"
  SpanishGuatemala   -> "es-gt"
  SpanishHonduras    -> "es-hn"
  SpanishMexico      -> "es-mx"
  SpanishNicaragua   -> "es-ni"
  SpanishPanama      -> "es-pa"
  SpanishParaguay    -> "es-py"
  SpanishPeru        -> "es-pe"
  SpanishPuertoRico  -> "es-pr"
  SpanishSpain       -> "es-es"
  SpanishUruguay     -> "es-uy"
  SpanishVenezuela   -> "es-ve"
  Swedish            -> "sv"
  SwedishFinland     -> "sv-fi"
  SwedishSweden      -> "sv-se"
  Turkish            -> "tr"
  Ukrainian          -> "uk"

-- | Format feed date according to RFC 822.

formatFeedDate :: UTCTime -> String
formatFeedDate = formatTime defaultTimeLocale "%a, %d %b %0Y %X GMT"
