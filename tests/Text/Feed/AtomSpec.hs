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
spec = do
  describe "Atom feed" $ do
    it "creates an Atom feed" $ do
      let atomFeed = testingAtomFeed
        
      traceShowId(Atom.renderFeed atomFeed) == "fixme"
    
testingAtomFeed :: Atom.Feed
testingAtomFeed =
  Atom.Feed
    { Atom.feedTitle = "Test title"
    , Atom.feedTitleType = Just Atom.TextType
    , Atom.feedAuthors = [testingAuthor "-1", testingAuthor "-2"]
    }

testingAuthor :: Text -> Atom.Person
testingAuthor suffix =
  Atom.Person
    { Atom.personName = "Some Guy" <> suffix
    , Atom.personEmail = Just ("someguy@some.guy" <> suffix)
    , Atom.personUri = Just ("some.guy" <> suffix)
    }