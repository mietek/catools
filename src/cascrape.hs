--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Exception (SomeException, handle)
import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, liftIO)
import Control.Lens ((&), (.=), (.~), (<~), (^.), makeLenses, use)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isDigit)
import Data.Csv ((.!), FromRecord, ToRecord, encode, parseRecord)
import Data.Csv.Streaming (HasHeader (NoHeader), Records (Nil, Cons), decode)
import Data.Decimal (Decimal)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, listToMaybe)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Network.HTTP.Client (CookieJar, createCookieJar)
import Network.Wreq (FormParam ((:=)))
import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)
import Text.HTML.TagSoup ((~/=), fromAttrib, fromTagText, parseTags)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector as V
import qualified Network.Wreq as W
import qualified Text.HTML.TagSoup as S

--------------------------------------------------------------------------------

type Tag = S.Tag ByteString

type Session a = StateT SessionState IO a

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

makeLenses ''SessionState

--------------------------------------------------------------------------------

data Transaction = Transaction
    { _transactionDate      :: !ByteString
    , _transactionReference :: !ByteString
    , _transactionDetails   :: !ByteString
    , _transactionAmount    :: !ByteString
    , _transactionBalance   :: !ByteString
    }
  deriving Generic

makeLenses ''Transaction

instance FromRecord Transaction
  where
    parseRecord v
        | V.length v == 5 =
            Transaction <$>
                  v .! 0
              <*> v .! 1
              <*> v .! 2
              <*> v .! 3
              <*> v .! 4
        | otherwise =
              empty

instance ToRecord Transaction

processTransactions :: Records Transaction -> IO ()
processTransactions (Cons (Right tr) more) = do
    L.putStr (encode [tr])
    processTransactions more
processTransactions (Cons (Left err) more) = do
    hPutStrLn stderr ("processTransactions: " ++ err)
    processTransactions more
processTransactions (Nil (Just err) rest) = do
    hPutStrLn stderr ("processTransactions: " ++ err)
    L.putStrLn rest
processTransactions (Nil Nothing rest) =
    L.putStrLn rest

--------------------------------------------------------------------------------

main :: IO ()
main = do
    (user, code, pass, from, to) <- getOpts
    csv <- runInSession user code pass (getTransactionsAsCsv from to)
    processTransactions (decode NoHeader csv)

getOpts :: IO (String, String, String, Day, Day)
getOpts =
    handle showUsage $
      getArgs >>= \case
        [fromDate, toDate] -> do
          user <- getEnv "CA_USER_NAME"
          code <- getEnv "CA_SECRET_ACCESS_CODE"
          pass <- getEnv "CA_SECRET_PASSWORD"
          return (user, code, pass, read fromDate, read toDate)
        _ ->
          error "FROM_DATE TO_DATE: getArgs: unexpected format"
  where
    showUsage err = do
      name <- getProgName
      hPutStr stderr $ unlines
        [ show (err :: SomeException)
        , ""
        , "Usage: " ++ name ++ " YYYY-MM-DD YYYY-MM-DD"
        , ""
        , "Environment variables:"
        , "  CA_USER_NAME           Cater Allen user name"
        , "  CA_SECRET_ACCESS_CODE  Cater Allen secret access code"
        , "  CA_SECRET_PASSWORD     Cater Allen secret password"
        ]
      exitFailure

--------------------------------------------------------------------------------

runInSession :: String -> String -> String -> Session a -> IO a
runInSession user code pass act =
    flip evalStateT (initState user code pass) $ do
      login
      res <- act
      logout
      return res

login :: Session ()
login = do
    _ <- liftIO (W.get baseUrl)
    access []
    user <- use userName
    code <- use secretAccessCode
    (c1, c2, c3) <- scrapeSecret codeChallenge code
    access
      [ "UserID" := user
      , "pwd1"   := c1
      , "pwd2"   := c2
      , "pwd3"   := c3
      ]
    pass <- use secretPassword
    (p1, p2, p3) <- scrapeSecret passChallenge pass
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
    continueTag     = "<input type=submit name=Menu value=Continue>"
    ownerTag        = "<div id=name_right_header_border"
    accountTag      = "<input type=submit name=Acc>"
    codeChallenge n = "<label for=ipos" ++ show n ++ ">"
    passChallenge n = "<label for=pos"  ++ show n ++ ">"

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

initState :: String -> String -> String -> SessionState
initState user code pass =
    SessionState
      { _userName         = user
      , _secretAccessCode = code
      , _secretPassword   = pass
      , _cookieJar        = createCookieJar []
      , _responseBody     = L.empty
      , _responseTags     = []
      , _responseToken    = L.empty
      , _accountOwner     = L.empty
      , _accountNumber    = L.empty
      , _accountName      = L.empty
      , _currentBalance   = 0
      }

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

scrapeSecret :: (Int -> String) -> String -> Session (String, String, String)
scrapeSecret challengeTag secret = do
    i1 <- readIndex <$> scrapeTagTextAfter 1 (challengeTag 1)
    i2 <- readIndex <$> scrapeTagTextAfter 1 (challengeTag 2)
    i3 <- readIndex <$> scrapeTagTextAfter 1 (challengeTag 3)
    return ([secret !! i1], [secret !! i2], [secret !! i3])
  where
    readIndex str = read (L.unpack str) - 1

--------------------------------------------------------------------------------

scrapeTag :: String -> Session Tag
scrapeTag like =
    tryScrapeTag like >>= \case
      Just tag -> return tag
      Nothing  -> error (like ++ ": scrapeTag: not found")

scrapeTagTextAfter :: Int -> String -> Session ByteString
scrapeTagTextAfter n like =
    fromTagText <$> scrapeTagAfter n like

scrapeTagAfter :: Int -> String -> Session Tag
scrapeTagAfter n like =
    tryScrapeTagAfter n like >>= \case
      Just tag -> return tag
      Nothing  -> error (like ++ ": scrapeTagAfter: not found")

tryScrapeTag :: String -> Session (Maybe Tag)
tryScrapeTag like =
    tryScrapeTagAfter 0 like

tryScrapeTagAfter :: Int -> String -> Session (Maybe Tag)
tryScrapeTagAfter n like = do
    tags <- use responseTags
    return (listToMaybe (drop n (dropWhile (~/= like) tags)))

--------------------------------------------------------------------------------

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predAct act =
    predAct >>= flip when act

--------------------------------------------------------------------------------
