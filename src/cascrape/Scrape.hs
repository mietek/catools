--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Scrape where

import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (listToMaybe)
import Text.HTML.TagSoup ((~/=), fromTagText)

import Scrape.Types

--------------------------------------------------------------------------------

scrapeTag :: (HasResponseTags s m) => String -> m Tag
scrapeTag like =
    tryScrapeTag like >>= \case
      Just tag -> return tag
      Nothing  -> error (like ++ ": scrapeTag: not found")

scrapeTagTextAfter :: (HasResponseTags s m) => Int -> String -> m ByteString
scrapeTagTextAfter n like =
    fromTagText <$> scrapeTagAfter n like

scrapeTagAfter :: (HasResponseTags s m) => Int -> String -> m Tag
scrapeTagAfter n like =
    tryScrapeTagAfter n like >>= \case
      Just tag -> return tag
      Nothing  -> error (like ++ ": scrapeTagAfter: not found")

tryScrapeTag :: (HasResponseTags s m) => String -> m (Maybe Tag)
tryScrapeTag like =
    tryScrapeTagAfter 0 like

tryScrapeTagAfter :: (HasResponseTags s m) => Int -> String -> m (Maybe Tag)
tryScrapeTagAfter n like = do
    tags <- useResponseTags
    return (listToMaybe (drop n (dropWhile (~/= like) tags)))

--------------------------------------------------------------------------------
