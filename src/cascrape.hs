--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, liftIO)
import Control.Lens ((.=), (^.), makeLenses, set, use)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (isJust, listToMaybe)
import Network.HTTP.Client (CookieJar, createCookieJar)
import Network.Wreq (FormParam ((:=)), cookies, defaults, getWith, postWith,
  responseBody, responseCookieJar)
import System.Environment (getEnv)
import Text.HTML.TagSoup ((~/=), fromAttrib, fromTagText, parseTags)

import qualified Data.ByteString.Lazy.Char8 as Lbs
import qualified Text.HTML.TagSoup as Soup

--------------------------------------------------------------------------------

type Tag = Soup.Tag ByteString

data SessionState = SessionState
    { _sessionCookies        :: CookieJar
    , _sessionResponseBody   :: ByteString
    , _sessionResponseTags   :: [Tag]
    , _sessionToken          :: ByteString
    , _sessionAccountOwner   :: ByteString
    , _sessionAccountNumber  :: ByteString
    , _sessionAccountName    :: ByteString
    , _sessionAccountBalance :: ByteString
    }

makeLenses ''SessionState

type Session a = StateT SessionState IO a

--------------------------------------------------------------------------------

main :: IO ()
main = do
    user <- getEnv "CA_USERNAME"
    code <- getEnv "CA_ACCESS_CODE"
    pass <- getEnv "CA_PASSWORD"
    flip evalStateT emptySession $ do
      login user code pass
      accOwn  <- use sessionAccountOwner
      accNum  <- use sessionAccountNumber
      accName <- use sessionAccountName
      accBal  <- use sessionAccountBalance
      liftIO $ do
        putMark
        Lbs.putStrLn (Lbs.append "Account owner:    " accOwn)
        Lbs.putStrLn (Lbs.append "Account number:   " accNum)
        Lbs.putStrLn (Lbs.append "Account name:     " accName)
        Lbs.putStrLn (Lbs.append "Account balance:  " accBal)
        putMark
      res <- downloadTransactions
      liftIO $ do
        Lbs.putStr res
        putMark
      logout
  where
    putMark = putStrLn (replicate 80 '-')

--------------------------------------------------------------------------------

baseUrl :: String
baseUrl = "https://www.caterallenonline.co.uk"

mainUrl :: String
mainUrl = baseUrl ++ "/WebAccess.dll"

login :: String -> String -> String -> Session ()
login user code pass = do
    getWithSession baseUrl
    postWithSession mainUrl []
    tags1 <- use sessionResponseTags
    let (c1, c2, c3) = fetchParts code tags1 mkCodeTag
    postWithSession mainUrl
      [ "UserID" := user
      , "pwd1"   := c1
      , "pwd2"   := c2
      , "pwd3"   := c3
      ]
    tags2 <- use sessionResponseTags
    let (p1, p2, p3) = fetchParts pass tags2 mkPassTag
    postWithSession mainUrl
      [ "pwd1" := p1
      , "pwd2" := p2
      , "pwd3" := p3
      ]
    tags3 <- use sessionResponseTags
    when (isJust (findTag tags3 conTag)) $
      postWithSession mainUrl
        [ "Menu" := ("Continue" :: String)
        ]
    tags4 <- use sessionResponseTags
    let accOwn  = fetchTagTextAfter tags4 3 ownTag
        accNum  = fetchTagValue tags4 accTag
        accName = fetchTagTextAfter tags4 8 accTag
        accBal  = fetchTagTextAfter tags4 12 accTag
    sessionAccountOwner   .= accOwn
    sessionAccountNumber  .= accNum
    sessionAccountName    .= accName
    sessionAccountBalance .= accBal
  where
    conTag = "<input type=submit name=Menu value=Continue>"
    ownTag = "<div id=name_right_header_border"
    accTag = "<input type=submit name=Acc>"
    mkCodeTag n = "<label for=ipos" ++ show n ++ ">"
    mkPassTag n = "<label for=pos"  ++ show n ++ ">"

logout :: Session ()
logout = do
    postWithSession mainUrl
      [ "Menu" := ("Log Out" :: String)
      ]

downloadTransactions :: Session ByteString
downloadTransactions = do
    accNum <- use sessionAccountNumber
    postWithSession mainUrl
      [ "Acc" := accNum
      ]
    postWithSession mainUrl
      [ "Menu" := ("Download Transaction Details" :: String)
      ]
    postWithSession mainUrl
      [ "Menu"       := ("Accept" :: String)
      , "fDay"       := ("01" :: String)
      , "fMonth"     := ("07" :: String)
      , "fYear"      := ("2014" :: String)
      , "tDay"       := ("31" :: String)
      , "tMonth"     := ("07" :: String)
      , "tYear"      := ("2014" :: String)
      , "FileFormat" := ("EXCEL" :: String)
      ]
    postWithSession mainUrl
      [ "Menu" := ("Download" :: String)
      ]
    use sessionResponseBody

--------------------------------------------------------------------------------

emptySession :: SessionState
emptySession =
    SessionState
      { _sessionCookies        = createCookieJar []
      , _sessionResponseBody   = Lbs.empty
      , _sessionResponseTags   = []
      , _sessionToken          = Lbs.empty
      , _sessionAccountOwner   = Lbs.empty
      , _sessionAccountNumber  = Lbs.empty
      , _sessionAccountName    = Lbs.empty
      , _sessionAccountBalance = Lbs.empty
      }

getWithSession :: String -> Session ()
getWithSession url = do
    jar <- use sessionCookies
    let opts = set cookies jar defaults
    res <- liftIO (getWith opts url)
    let body = res ^. responseBody
        tags = parseTags body
    sessionCookies      .= res ^. responseCookieJar
    sessionResponseBody .= body
    sessionResponseTags .= tags
    sessionToken        .= findTagValue tags tokTag
  where
    tokTag = "<input type=hidden name=Trxn>"

postWithSession :: String -> [FormParam] -> Session ()
postWithSession url params = do
    jar <- use sessionCookies
    tok <- use sessionToken
    let opts = set cookies jar defaults
    res <- liftIO (postWith opts url (("Trxn" := tok) : params))
    let body = res ^. responseBody
        tags = parseTags body
    sessionCookies      .= res ^. responseCookieJar
    sessionResponseBody .= body
    sessionResponseTags .= tags
    sessionToken        .= findTagValue tags tokTag
  where
    tokTag = "<input type=hidden name=Trxn>"

--------------------------------------------------------------------------------

findTag :: [Tag] -> String -> Maybe Tag
findTag tags tag =
    listToMaybe (dropWhile (~/= tag) tags)

findTagValue :: [Tag] -> String -> ByteString
findTagValue tags tag =
    maybe Lbs.empty (fromAttrib "value") (findTag tags tag)

fetchTag :: [Tag] -> String -> Tag
fetchTag tags tag =
    head (dropWhile (~/= tag) tags)

fetchTagValue :: [Tag] -> String -> ByteString
fetchTagValue tags tag =
    fromAttrib "value" (fetchTag tags tag)

fetchTagAfter :: [Tag] -> Int -> String -> Tag
fetchTagAfter tags n tag =
    head (drop n (dropWhile (~/= tag) tags))

fetchTagTextAfter :: [Tag] -> Int -> String -> ByteString
fetchTagTextAfter tags n tag =
    fromTagText (fetchTagAfter tags n tag)

fetchParts :: String -> [Tag] -> (Int -> String) -> (String, String, String)
fetchParts whole tags mkTag =
    (wrap (fetch 1), wrap (fetch 2), wrap (fetch 3))
  where
    wrap s  = [whole !! (read s - 1)]
    fetch n = Lbs.unpack (fetchTagTextAfter tags 1 (mkTag n))

--------------------------------------------------------------------------------
