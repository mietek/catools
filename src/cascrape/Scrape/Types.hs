--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scrape.Types where

import Control.Lens (makeLenses, use)
import Control.Monad.State (MonadState, StateT)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Decimal (Decimal)
import Network.HTTP.Client (CookieJar, createCookieJar)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Text.HTML.TagSoup as S

--------------------------------------------------------------------------------

type Tag = S.Tag ByteString

class (Functor m, MonadState s m) => HasTags s m | m -> s
  where
    useTags :: m [Tag]

--------------------------------------------------------------------------------

type Session = StateT SessionState IO

data SessionState = SessionState
    { _userName         :: !String
    , _secretAccessCode :: !String
    , _secretPassword   :: !String
    , _cookieJar        :: !CookieJar
    , _responseBody     :: !ByteString
    , _responseTags     :: ![Tag]
    , _responseToken    :: !ByteString
    , _accountOwner     :: !ByteString
    , _accountNumber    :: !ByteString
    , _accountName      :: !ByteString
    , _currentBalance   :: !Decimal
    }
  deriving (Show)

makeLenses ''SessionState

instance HasTags SessionState Session
  where
    useTags = use responseTags

emptySessionState :: SessionState
emptySessionState =
    SessionState
      { _userName         = ""
      , _secretAccessCode = ""
      , _secretPassword   = ""
      , _cookieJar        = createCookieJar []
      , _responseBody     = L.empty
      , _responseTags     = []
      , _responseToken    = L.empty
      , _accountOwner     = L.empty
      , _accountNumber    = L.empty
      , _accountName      = L.empty
      , _currentBalance   = 0
      }

--------------------------------------------------------------------------------
