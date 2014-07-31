--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.State (StateT, evalStateT, liftIO)
import Control.Lens (assign, makeLenses, set, use, view)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (isJust, listToMaybe)
import Network.Wreq (FormParam)
import System.Environment (getEnv)
import Text.HTML.TagSoup ((~/=))

import qualified Data.ByteString.Char8 as Sbs
import qualified Data.ByteString.Lazy.Char8 as Lbs
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import qualified Text.HTML.TagSoup as S

--------------------------------------------------------------------------------

baseUrl :: String
baseUrl =
    "https://www.caterallenonline.co.uk"

mainUrl :: String
mainUrl =
    baseUrl ++ "/WebAccess.dll"

--------------------------------------------------------------------------------

findTag :: [S.Tag ByteString] -> String -> Maybe (S.Tag ByteString)
findTag tags tag =
    listToMaybe (dropWhile (~/= tag) tags)

findTagValue :: [S.Tag ByteString] -> String -> Maybe (ByteString)
findTagValue tags tag =
    S.fromAttrib "value" `fmap` findTag tags tag

fetchTag :: [S.Tag ByteString] -> String -> S.Tag ByteString
fetchTag tags tag =
    head (dropWhile (~/= tag) tags)

fetchTagValue :: [S.Tag ByteString] -> String -> ByteString
fetchTagValue tags tag =
    S.fromAttrib "value" (fetchTag tags tag)

fetchTagAfter :: [S.Tag ByteString] -> Int -> String -> S.Tag ByteString
fetchTagAfter tags n tag =
    head (drop n (dropWhile (~/= tag) tags))

fetchTagTextAfter :: [S.Tag ByteString] -> Int -> String -> ByteString
fetchTagTextAfter tags n tag =
    S.fromTagText (fetchTagAfter tags n tag)

fetchParts :: String -> [S.Tag ByteString] -> (Int -> String) -> (String, String, String)
fetchParts whole tags mkTag =
    (wrap (fetch 1), wrap (fetch 2), wrap (fetch 3))
  where
    wrap s  = [whole !! (read s - 1)]
    fetch n = Lbs.unpack (fetchTagTextAfter tags 1 (mkTag n))

--------------------------------------------------------------------------------

type Session a = StateT SessionState IO a

data SessionState = SessionState
    { _sessionCookies        :: H.CookieJar
    , _sessionToken          :: Maybe String
    , _sessionAccountOwner   :: Maybe String
    , _sessionAccountNumber  :: Maybe String
    , _sessionAccountName    :: Maybe String
    , _sessionAccountBalance :: Maybe String
    }

makeLenses ''SessionState

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

getWithSession :: String -> Session (W.Response ByteString, [S.Tag ByteString])
getWithSession url = do
    jar <- use sessionCookies
    res <- liftIO (W.getWith (set W.cookies jar W.defaults) url)
    assign sessionCookies (view W.responseCookieJar res)
    let tags = parseTags res
    return (res, tags)

postWithSession :: String -> [FormParam] -> Session (W.Response ByteString, [S.Tag ByteString])
postWithSession url params = do
    jar <- use sessionCookies
    mtran <- use sessionToken
    let extraParam = case mtran of
            Just tran -> [ param "Trxn" tran ]
            Nothing   -> []
    let params' = extraParam ++ params
    res <- liftIO (W.postWith (set W.cookies jar W.defaults) url params')
    assign sessionCookies (view W.responseCookieJar res)
    let tags = parseTags res
    assign sessionToken (Lbs.unpack `fmap` findTagValue tags transactionTag)
    return (res, tags)
  where
    transactionTag = "<input type=hidden name=Trxn>"

param :: Sbs.ByteString -> String -> FormParam
param name str =
    name W.:= str

parseTags :: W.Response ByteString -> [S.Tag ByteString]
parseTags res =
    S.parseTags (view W.responseBody res)

--------------------------------------------------------------------------------

login :: String -> String -> String -> Session ()
login user code pass = do
    _ <- getWithSession baseUrl
    (_, tags1) <- postWithSession mainUrl []
    let (c1, c2, c3) = fetchParts code tags1 mkCode
    (_, tags2) <- postWithSession mainUrl
      [ param "UserID" user
      , param "pwd1"   c1
      , param "pwd2"   c2
      , param "pwd3"   c3
      ]
    let (p1, p2, p3) = fetchParts pass tags2 mkPass
    (_, tags3) <- postWithSession mainUrl
      [ param "pwd1" p1
      , param "pwd2" p2
      , param "pwd3" p3
      ]
    tags4 <- if isJust (findTag tags3 menu)
    then do
      (_, tags') <- postWithSession mainUrl
        [ param "Menu" "Continue"
        ]
      return tags'
    else
      return tags3
    let accOwn  = Lbs.unpack (fetchTagTextAfter tags4 3 own)
        accNum  = Lbs.unpack (fetchTagValue tags4 acc)
        accName = Lbs.unpack (fetchTagTextAfter tags4 8 acc)
        accBal  = Lbs.unpack (fetchTagTextAfter tags4 12 acc)
    assign sessionAccountOwner   (Just accOwn)
    assign sessionAccountNumber  (Just accNum)
    assign sessionAccountName    (Just accName)
    assign sessionAccountBalance (Just accBal)
  where
    mkCode n = "<label for=ipos" ++ show n ++ " class=tran_confirm>"
    mkPass n = "<label for=pos"  ++ show n ++ " class=tran_confirm>"
    menu = "<input type=submit name=Menu value=Continue>"
    own  = "<div id=name_right_header_border class=name_right_header_border>"
    acc  = "<input type=submit name=Acc>"

logout :: Session ()
logout = do
    _ <- postWithSession mainUrl
      [ param "Menu" "Log Out"
      ]
    return ()

downloadTransactions :: Session ByteString
downloadTransactions = do
    Just accNum <- use sessionAccountNumber
    _ <- postWithSession mainUrl
      [ param "Acc" accNum
      ]
    _ <- postWithSession mainUrl
      [ param "Menu" "Download Transaction Details"
      ]
    _ <- postWithSession mainUrl
      [ param "Menu"       "Accept"
      , param "fDay"       "01"
      , param "fMonth"     "07"
      , param "fYear"      "2014"
      , param "tDay"       "31"
      , param "tMonth"     "07"
      , param "tYear"      "2014"
      , param "FileFormat" "EXCEL"
      ]
    (res, _) <- postWithSession mainUrl
      [ param "Menu" "Download"
      ]
    return (view W.responseBody res)

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
