{-# LANGUAGE OverloadedStrings #-}

module Text.Feed.AtomSpec
  ( spec )
where

import Data.Monoid ((<>))
import Data.Text (Text)
import Test.Hspec

import qualified Text.Feed.Atom as Atom

import Debug.Trace

spec :: Spec
spec =
  describe "Atom feed" $ do
    it "creates an Atom feed" $ do
      let atomFeed = testingAtomFeed
      traceShowId(Atom.renderFeed atomFeed) == "fixme"
    
testingAtomFeed :: Atom.Feed
testingAtomFeed =
  Atom.Feed
    { Atom.feedTitle = "Test title"
    , Atom.feedTitleType = Just Atom.TextType
    , Atom.feedAuthors = [testingPerson "-au1", testingPerson "-au2"]
    , Atom.feedCategories = [testingCategory "-a", testingCategory "-b"]
    , Atom.feedContributors = [testingPerson "-co1", testingPerson "-co2"]
    , Atom.feedGenerator = Just (Atom.Generator (Just "genUri") (Just "ver1.1") "gen text")
    , Atom.feedIcon = Just (Atom.Icon "someiconUri")
    , Atom.feedId = "someFeedId"
    , Atom.feedLinks = [testingLink "1", testingLink "2"]
    }

testingPerson :: Text -> Atom.Person
testingPerson suffix =
  Atom.Person
    { Atom.personName = "Some Guy" <> suffix
    , Atom.personEmail = Just ("someguy@some.guy" <> suffix)
    , Atom.personUri = Just ("some.guy" <> suffix)
    }

testingCategory :: Text -> Atom.Category
testingCategory suffix =
  Atom.Category
    { Atom.categoryTerm = "catTerm" <> suffix
    , Atom.categoryScheme = Just ("catScheme" <> suffix)
    , Atom.categoryLabel = Just ("catLabel" <> suffix)
    }

testingLink :: Text -> Atom.Link
testingLink suffix =
  Atom.Link
    { Atom.linkHref = "linkhref" <> suffix
    , Atom.linkRel = Just ("linkrel" <> suffix)
    , Atom.linkType = Just ("linktype" <> suffix)
    , Atom.linkHrefLang = Just ("linkhreflang" <> suffix)
    , Atom.linkTitle = Just ("linktitle" <> suffix)
    , Atom.linkLength = Just ("linklength" <> suffix)
    }