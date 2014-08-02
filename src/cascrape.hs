--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, handle)
import Control.Monad (when)
import Control.Monad.State (evalStateT, liftIO)
import Control.Lens ((&), (.=), (.~), (<~), (^.), use)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Time.Calendar (Day)
import Network.Wreq (FormParam ((:=)))
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)
import Text.HTML.TagSoup (fromAttrib, parseTags)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.Wreq as W

import Scrape
import Scrape.Types

--------------------------------------------------------------------------------

main :: IO ()
main = do
    (user, code, pass, from, to) <- getOpts
    csv <- runSession user code pass (getTransactionsAsCsv from to)
    L.putStr csv

getOpts :: IO (String, String, String, Day, Day)
getOpts =
    handle exitWithUsage $
      getArgs >>= \case
        [from, to] -> do
          user <- getEnv "CA_USER_NAME"
          code <- getEnv "CA_SECRET_ACCESS_CODE"
          pass <- getEnv "CA_SECRET_PASSWORD"
          return (user, code, pass, read from, read to)
        _ ->
          error "FROM_DATE TO_DATE: getOpts: unexpected format"
  where
    exitWithUsage err = do
      hPutStr stderr $ unlines
        [ show (err :: SomeException)
        , ""
        , "Usage: cascrape FROM_DATE TO_DATE"
        , ""
        , "Arguments:"
        , "  FROM_DATE  Transactions start (YYYY-MM-DD)"
        , "  TO_DATE    Transactions end   (YYYY-MM-DD)"
        , ""
        , "Environment variables:"
        , "  CA_USER_NAME           Cater Allen user name"
        , "  CA_SECRET_ACCESS_CODE  Cater Allen secret access code"
        , "  CA_SECRET_PASSWORD     Cater Allen secret password"
        ]
      exitFailure

--------------------------------------------------------------------------------

runSession :: String -> String -> String -> Session a -> IO a
runSession user code pass act = do
    let s = emptySessionState
          & userName         .~ user
          & secretAccessCode .~ code
          & secretPassword   .~ pass
    flip evalStateT s $ do
      login
      res <- act
      logout
      return res

--------------------------------------------------------------------------------

login :: Session ()
login = do
    _ <- liftIO (W.get baseUrl)
    access []
    user <- use userName
    code <- use secretAccessCode
    (c1, c2, c3) <- splitSecretBy code codeTag
    access
      [ "UserID" := user
      , "pwd1"   := c1
      , "pwd2"   := c2
      , "pwd3"   := c3
      ]
    pass <- use secretPassword
    (p1, p2, p3) <- splitSecretBy pass passTag
    access
      [ "pwd1" := p1
      , "pwd2" := p2
      , "pwd3" := p3
      ]
    whenM (isJust <$> tryScrapeTag continueTag) $
      access
        [ "Menu" := ("Continue" :: String)
        ]
    accountOwner   <~ scrapeTagTextAfter 3 ownerTag
    accountNumber  <~ fromAttrib "value" <$> scrapeTag accountTag
    accountName    <~ scrapeTagTextAfter 8 accountTag
    curBal <- L.unpack <$> scrapeTagTextAfter 12 accountTag
    currentBalance .= read (filter (\c -> isDigit c || c == '.') curBal)
  where
    continueTag = "<input type=submit name=Menu value=Continue>"
    ownerTag    = "<div id=name_right_header_border"
    accountTag  = "<input type=submit name=Acc>"
    codeTag n   = "<label for=ipos" ++ show n ++ ">"
    passTag n   = "<label for=pos"  ++ show n ++ ">"

logout :: Session ()
logout = do
    access
      [ "Menu" := ("Log Out" :: String)
      ]

getTransactionsAsCsv :: Day -> Day -> Session ByteString
getTransactionsAsCsv from to = do
    accNum <- use accountNumber
    access
      [ "Acc" := accNum
      ]
    access
      [ "Menu" := ("Download Transaction Details" :: String)
      ]
    access
      [ "Menu"       := ("Accept" :: String)
      , "fDay"       := fd
      , "fMonth"     := fm
      , "fYear"      := fy
      , "tDay"       := td
      , "tMonth"     := tm
      , "tYear"      := ty
      , "FileFormat" := ("EXCEL" :: String)
      ]
    access
      [ "Menu" := ("Download" :: String)
      ]
    use responseBody
  where
    [fy, fm, fd] = splitOn "-" (show from)
    [ty, tm, td] = splitOn "-" (show to)

--------------------------------------------------------------------------------

baseUrl :: String
baseUrl =
    "https://www.caterallenonline.co.uk"

accessUrl :: String
accessUrl =
    baseUrl ++ "/WebAccess.dll"

access :: [FormParam] -> Session ()
access params = do
    jar   <- use cookieJar
    token <- use responseToken
    let opts      = W.defaults & W.cookies .~ jar
        allParams = ("Trxn" := token) : params
    response <- liftIO (W.postWith opts accessUrl allParams)
    cookieJar    .= response ^. W.responseCookieJar
    responseBody .= response ^. W.responseBody
    responseTags .= parseTags (response ^. W.responseBody)
    tryScrapeTag tokenTag >>= \case
      Just tag -> responseToken .= fromAttrib "value" tag
      Nothing  -> responseToken .= L.empty
  where
    tokenTag = "<input type=hidden name=Trxn>"

splitSecretBy :: String -> (Int -> String) -> Session (String, String, String)
splitSecretBy secret tag = do
    i1 <- readIndex <$> scrapeTagTextAfter 1 (tag 1)
    i2 <- readIndex <$> scrapeTagTextAfter 1 (tag 2)
    i3 <- readIndex <$> scrapeTagTextAfter 1 (tag 3)
    return ([secret !! i1], [secret !! i2], [secret !! i3])
  where
    readIndex str = read (L.unpack str) - 1

--------------------------------------------------------------------------------

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predAct act =
    predAct >>= flip when act

--------------------------------------------------------------------------------
