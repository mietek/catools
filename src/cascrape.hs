--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.State (StateT, evalStateT, liftIO)
import Control.Lens ((.=), (^.), makeLenses, set, use)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (isJust, listToMaybe)
import Network.Wreq (FormParam ((:=)), cookies, defaults, getWith, postWith,
  responseBody, responseCookieJar)
import System.Environment (getEnv)
import Text.HTML.TagSoup ((~/=), fromAttrib, fromTagText, parseTags)

import qualified Data.ByteString.Lazy.Char8 as Lbs
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import qualified Text.HTML.TagSoup as S

--------------------------------------------------------------------------------

data SessionState = SessionState
    { _sessionCookies        :: H.CookieJar
    , _sessionToken          :: Maybe String
    , _sessionAccountOwner   :: Maybe String
    , _sessionAccountNumber  :: Maybe String
    , _sessionAccountName    :: Maybe String
    , _sessionAccountBalance :: Maybe String
    }

makeLenses ''SessionState

type Session a = StateT SessionState IO a

type Response = W.Response ByteString

type Tag = S.Tag ByteString

--------------------------------------------------------------------------------

emptySession :: SessionState
emptySession =
    SessionState
      { _sessionCookies        = H.createCookieJar []
      , _sessionToken          = Nothing
      , _sessionAccountOwner   = Nothing
      , _sessionAccountNumber  = Nothing
      , _sessionAccountName    = Nothing
      , _sessionAccountBalance = Nothing
      }

baseUrl :: String
baseUrl = "https://www.caterallenonline.co.uk"

mainUrl :: String
mainUrl = baseUrl ++ "/WebAccess.dll"

--------------------------------------------------------------------------------

main :: IO ()
main = do
    user <- getEnv "CA_USERNAME"
    code <- getEnv "CA_ACCESS_CODE"
    pass <- getEnv "CA_PASSWORD"
    flip evalStateT emptySession $ do
      login user code pass
      Just accOwn  <- use sessionAccountOwner
      Just accNum  <- use sessionAccountNumber
      Just accName <- use sessionAccountName
      Just accBal  <- use sessionAccountBalance
      liftIO $ do
        putMark
        putStrLn ("Account owner:    " ++ accOwn)
        putStrLn ("Account number:   " ++ accNum)
        putStrLn ("Account name:     " ++ accName)
        putStrLn ("Account balance:  " ++ accBal)
        putMark
      res <- downloadTransactions
      liftIO $ do
        Lbs.putStr res
        putMark
      logout
  where
    putMark = putStrLn (replicate 80 '-')

--------------------------------------------------------------------------------

login :: String -> String -> String -> Session ()
login user code pass = do
    _ <- getWithSession baseUrl
    (_, tags1) <- postWithSession mainUrl []
    let (c1, c2, c3) = fetchParts code tags1 mkCodeTag
    (_, tags2) <- postWithSession mainUrl
      [ "UserID" := user
      , "pwd1"   := c1
      , "pwd2"   := c2
      , "pwd3"   := c3
      ]
    let (p1, p2, p3) = fetchParts pass tags2 mkPassTag
    (_, tags3) <- postWithSession mainUrl
      [ "pwd1" := p1
      , "pwd2" := p2
      , "pwd3" := p3
      ]
    tags4 <- if isJust (findTag tags3 conTag)
      then do
        (_, tags') <- postWithSession mainUrl
          [ "Menu" := ("Continue" :: String)
          ]
        return tags'
      else
        return tags3
    let accOwn  = Lbs.unpack (fetchTagTextAfter tags4 3 ownTag)
        accNum  = Lbs.unpack (fetchTagValue tags4 accTag)
        accName = Lbs.unpack (fetchTagTextAfter tags4 8 accTag)
        accBal  = Lbs.unpack (fetchTagTextAfter tags4 12 accTag)
    sessionAccountOwner   .= Just accOwn
    sessionAccountNumber  .= Just accNum
    sessionAccountName    .= Just accName
    sessionAccountBalance .= Just accBal
  where
    conTag = "<input type=submit name=Menu value=Continue>"
    ownTag = "<div id=name_right_header_border"
    accTag = "<input type=submit name=Acc>"
    mkCodeTag n = "<label for=ipos" ++ show n ++ ">"
    mkPassTag n = "<label for=pos"  ++ show n ++ ">"

logout :: Session ()
logout = do
    _ <- postWithSession mainUrl
      [ "Menu" := ("Log Out" :: String)
      ]
    return ()

downloadTransactions :: Session ByteString
downloadTransactions = do
    Just accNum <- use sessionAccountNumber
    _ <- postWithSession mainUrl
      [ "Acc" := accNum
      ]
    _ <- postWithSession mainUrl
      [ "Menu" := ("Download Transaction Details" :: String)
      ]
    _ <- postWithSession mainUrl
      [ "Menu"       := ("Accept" :: String)
      , "fDay"       := ("01" :: String)
      , "fMonth"     := ("07" :: String)
      , "fYear"      := ("2014" :: String)
      , "tDay"       := ("31" :: String)
      , "tMonth"     := ("07" :: String)
      , "tYear"      := ("2014" :: String)
      , "FileFormat" := ("EXCEL" :: String)
      ]
    (res, _) <- postWithSession mainUrl
      [ "Menu" := ("Download" :: String)
      ]
    return (res ^. responseBody)

--------------------------------------------------------------------------------

getWithSession :: String -> Session (Response, [Tag])
getWithSession url = do
    jar <- use sessionCookies
    let opts = set cookies jar defaults
    res <- liftIO (getWith opts url)
    sessionCookies .= res ^. responseCookieJar
    let tags = parseTags (res ^. responseBody)
    return (res, tags)

postWithSession :: String -> [FormParam] -> Session (Response, [Tag])
postWithSession url params = do
    jar <- use sessionCookies
    tok <- use sessionToken
    let opts = set cookies jar defaults
        tokParams = ("Trxn" := tok) : params
    res <- liftIO (postWith opts url tokParams)
    let tags = parseTags (res ^. responseBody)
    sessionCookies .= res ^. responseCookieJar
    sessionToken   .= Lbs.unpack `fmap` findTagValue tags tokTag
    return (res, tags)
  where
    tokTag = "<input type=hidden name=Trxn>"

--------------------------------------------------------------------------------

findTag :: [Tag] -> String -> Maybe Tag
findTag tags tag =
    listToMaybe (dropWhile (~/= tag) tags)

findTagValue :: [Tag] -> String -> Maybe (ByteString)
findTagValue tags tag =
    fromAttrib "value" `fmap` findTag tags tag

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
