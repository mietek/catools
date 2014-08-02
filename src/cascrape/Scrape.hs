--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Scrape where

import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (listToMaybe)
import Text.HTML.TagSoup ((~/=), fromAttrib, fromTagText)

import qualified Data.ByteString.Lazy.Char8 as L

import Scrape.Types

--------------------------------------------------------------------------------

scrapeAttrib :: (HasTags s m) => ByteString -> String -> m ByteString
scrapeAttrib attrib like =
    fromAttrib attrib <$> scrapeTag like

scrapeTextAfter :: (HasTags s m) => Int -> String -> m ByteString
scrapeTextAfter n like =
    fromTagText <$> scrapeTagAfter n like

--------------------------------------------------------------------------------

scrapeTag :: (HasTags s m) => String -> m Tag
scrapeTag like =
    tryScrapeTag like >>= \case
      Just tag -> return tag
      Nothing  -> error (like ++ ": scrapeTag: not found")

scrapeTagAfter :: (HasTags s m) => Int -> String -> m Tag
scrapeTagAfter n like =
    tryScrapeTagAfter n like >>= \case
      Just tag -> return tag
      Nothing  -> error (like ++ ": scrapeTagAfter: not found")

--------------------------------------------------------------------------------

scrapeAttribOrEmpty :: (HasTags s m) => ByteString -> String -> m ByteString
scrapeAttribOrEmpty attrib like =
    tryScrapeTag like >>= \case
      Just tag -> return (fromAttrib attrib tag)
      Nothing  -> return L.empty

--------------------------------------------------------------------------------

tryScrapeTag :: (HasTags s m) => String -> m (Maybe Tag)
tryScrapeTag like =
    tryScrapeTagAfter 0 like

tryScrapeTagAfter :: (HasTags s m) => Int -> String -> m (Maybe Tag)
tryScrapeTagAfter n like = do
    tags <- useTags
    return (listToMaybe (drop n (dropWhile (~/= like) tags)))

--------------------------------------------------------------------------------
