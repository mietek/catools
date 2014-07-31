--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.State (StateT, evalStateT, liftIO)
import Control.Lens ((&), (.=), (.~), (^.), makeLenses, use)
import Data.Maybe (isJust, listToMaybe)
import Network.Wreq (FormParam ((:=)))
import System.Environment (getEnv)
import Text.HTML.TagSoup ((~/=))

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import qualified Text.HTML.TagSoup as S

--------------------------------------------------------------------------------

data SessionState = SessionState
    { _sessionUsername       :: String
    , _sessionAccessCode     :: String
    , _sessionPassword       :: String
    , _sessionCookies        :: H.CookieJar
    , _sessionAccountNumber  :: Maybe String
    , _sessionAccountName    :: Maybe String
    , _sessionAccountBalance :: Maybe String
    }

makeLenses ''SessionState

initSession :: String -> String -> String -> SessionState
initSession user code pass =
    SessionState
      { _sessionUsername       = user
      , _sessionAccessCode     = code
      , _sessionPassword       = pass
      , _sessionCookies        = H.createCookieJar []
      , _sessionAccountNumber  = Nothing
      , _sessionAccountName    = Nothing
      , _sessionAccountBalance = Nothing
      }

--------------------------------------------------------------------------------

type Session a = StateT SessionState IO a

getWithSession :: String -> Session (W.Response L.ByteString)
getWithSession url = do
    jar <- use sessionCookies
    res <- liftIO $ W.getWith (cookie jar) url
    sessionCookies .= res ^. W.responseCookieJar
    return res

postWithSession :: String -> [FormParam] -> Session (W.Response L.ByteString)
postWithSession url params = do
    jar <- use sessionCookies
    res <- liftIO $ W.postWith (cookie jar) url params
    sessionCookies .= res ^. W.responseCookieJar
    return res

cookie :: H.CookieJar -> W.Options
cookie jar =
    W.defaults & W.cookies .~ jar

--------------------------------------------------------------------------------

parseTags :: W.Response L.ByteString -> [S.Tag L.ByteString]
parseTags res =
    S.parseTags $ res ^. W.responseBody

--------------------------------------------------------------------------------

findTag :: [S.Tag L.ByteString] -> String -> Maybe (S.Tag L.ByteString)
findTag tags tag =
    listToMaybe $ dropWhile (~/= tag) tags

getTag :: [S.Tag L.ByteString] -> String -> S.Tag L.ByteString
getTag tags tag =
    head $ dropWhile (~/= tag) tags

getTagValue :: [S.Tag L.ByteString] -> String -> L.ByteString
getTagValue tags tag =
    S.fromAttrib "value" $ getTag tags tag

getTagAfter :: [S.Tag L.ByteString] -> Int -> String -> S.Tag L.ByteString
getTagAfter tags n tag =
    head . drop n $ dropWhile (~/= tag) tags

getTagTextAfter :: [S.Tag L.ByteString] -> Int -> String -> L.ByteString
getTagTextAfter tags n tag =
    S.fromTagText $ getTagAfter tags n tag

--------------------------------------------------------------------------------

caBaseUrl :: String
caBaseUrl =
    "https://www.caterallenonline.co.uk"

caUrl :: String
caUrl =
    caBaseUrl ++ "/WebAccess.dll"

transactionTag :: String
transactionTag =
    "<input type=hidden name=Trxn>"

mkCodeTag :: Int -> String
mkCodeTag n =
    "<label for=ipos" ++ show n ++ " class=tran_confirm>"

mkPasswordTag :: Int -> String
mkPasswordTag n =
    "<label for=pos"  ++ show n ++ " class=tran_confirm>"

menuTag :: String
menuTag =
    "<input type=submit name=Menu value=Continue>"

accountTag :: String
accountTag =
    "<input type=submit name=Acc>"

--------------------------------------------------------------------------------

getParts :: String -> [S.Tag L.ByteString] -> (Int -> String) -> (String, String, String)
getParts whole tags mkTag =
    (wrap $ getPart 1, wrap $ getPart 2, wrap $ getPart 3)
  where
    wrap s    = [whole !! (read s - 1)]
    getPart n = L.unpack . getTagTextAfter tags 1 $ mkTag n

--------------------------------------------------------------------------------

login :: Session ()
login =
    step1
  where
    step1 = do
        res <- getWithSession caBaseUrl
        let tags = parseTags res
            tran = getTagValue tags transactionTag
        step2 tran

    step2 tran = do
        code <- use sessionAccessCode
        res <- postWithSession caUrl
          [ "Trxn" := tran
          ]
        let tags  = parseTags res
            tran' = getTagValue tags transactionTag
            parts = getParts code tags mkCodeTag
        step3 tran' parts

    step3 tran (p1, p2, p3) = do
        user <- use sessionUsername
        pass <- use sessionPassword
        res <- postWithSession caUrl
          [ "Trxn"   := tran
          , "UserID" := user
          , "pwd1"   := p1
          , "pwd2"   := p2
          , "pwd3"   := p3
          ]
        let tags  = parseTags res
            tran' = getTagValue tags transactionTag
            parts = getParts pass tags mkPasswordTag
        step4 tran' parts

    step4 tran (p1, p2, p3) = do
        res <- postWithSession caUrl
          [ "Trxn" := tran
          , "pwd1" := p1
          , "pwd2" := p2
          , "pwd3" := p3
          ]
        let tags = parseTags res
        if isJust $ findTag tags menuTag
          then step5
          else step6 tags

    step5 = do
        res <- postWithSession caUrl
          [ "Menu" := ("Continue" :: L.ByteString)
          ]
        let tags = parseTags res
        step6 tags

    step6 tags = do
        let accNum  = L.unpack $ getTagValue tags accountTag
            accName = L.unpack $ getTagTextAfter tags 8 accountTag
            accBal  = L.unpack $ getTagTextAfter tags 12 accountTag
        sessionAccountNumber  .= Just accNum
        sessionAccountName    .= Just accName
        sessionAccountBalance .= Just accBal

--------------------------------------------------------------------------------

main :: IO ()
main = do
    user <- getEnv "CA_USERNAME"
    code <- getEnv "CA_ACCESS_CODE"
    pass <- getEnv "CA_PASSWORD"
    let session = initSession user code pass
    evalStateT run session
  where
    run = do
      login
      Just accNum  <- use sessionAccountNumber
      Just accName <- use sessionAccountName
      Just accBal  <- use sessionAccountBalance
      liftIO . putStrLn $ "Account number:   " ++ accNum
      liftIO . putStrLn $ "Account name:     " ++ accName
      liftIO . putStrLn $ "Account balance:  " ++ accBal

--------------------------------------------------------------------------------
