--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import Control.Applicative ((<$>), pure)
import Control.Exception (SomeException, handle)
import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, liftIO)
import Control.Lens ((&), (.=), (.~), (<~), (^.), makeLenses, use)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isDigit, isSpace)
import Data.Csv (FromField, FromRecord, ToField, ToRecord, encode, parseField, toField)
import Data.Csv.Streaming (HasHeader (NoHeader), Records (Nil, Cons), decode)
import Data.Decimal (Decimal)
import Data.Maybe (isJust, listToMaybe)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Client (CookieJar, createCookieJar)
import Network.Wreq (FormParam ((:=)))
import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Locale (defaultTimeLocale)
import Text.HTML.TagSoup ((~/=), fromAttrib, fromTagText, parseTags)

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Time.Format as T
import qualified Network.Wreq as W
import qualified Text.HTML.TagSoup as S

--------------------------------------------------------------------------------

newtype Amount = Amount Decimal
  deriving (Eq, Num, Ord)

newtype Date = Date UTCTime

type Tag = S.Tag ByteString

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
    , _currentBalance   :: !Amount
    }

makeLenses ''SessionState

type Session a = StateT SessionState IO a

data Transaction = Transaction
    { _transactionDate      :: !Date
    , _transactionReference :: !ByteString
    , _transactionDetails   :: !ByteString
    , _transactionAmount    :: !Amount
    , _transactionBalance   :: !Amount
    }
  deriving Generic

makeLenses ''Transaction

--------------------------------------------------------------------------------

main :: IO ()
main = do
    (user, code, pass, from, to) <- getOpts
    csv <- runInSession user code pass (getTransactionsAsCsv from to)
    processTransactions (decode NoHeader csv)

getOpts :: IO (String, String, String, Date, Date)
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

instance Read Amount where
    readsPrec _ str =
        let (dirty, rest) = break isSpace str
            clean = filter (\c -> isDigit c || c == '-' || c == '.') dirty
        in [(Amount (read clean), rest)]

instance Show Amount where
    show (Amount amt) =
        show amt

instance FromField Amount where
    parseField str =
        pure (read (S.unpack str))

instance ToField Amount where
    toField amt =
        S.pack (show amt)

instance Read Date where
    readsPrec _ str =
        let (part, rest) = splitAt 10 str
        in [(Date (readTime "%Y-%m-%d" part), rest)]

instance Show Date where
    show (Date date) =
        formatTime "%Y-%m-%d" date

instance FromField Date where
    parseField str =
        pure (Date (readTime "%d%b%Y" (S.unpack str)))

instance ToField Date where
    toField date =
        S.pack (show date)

instance FromRecord Transaction

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
    currentBalance <~ read . L.unpack <$> scrapeTagTextAfter 12 accountTag
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

getTransactionsAsCsv :: Date -> Date -> Session ByteString
getTransactionsAsCsv (Date from) (Date to) = do
    accNum <- use accountNumber
    access
      [ "Acc" := accNum
      ]
    access
      [ "Menu" := ("Download Transaction Details" :: String)
      ]
    access
      [ "Menu"       := ("Accept" :: String)
      , "fDay"       := formatTime "%d" from
      , "fMonth"     := formatTime "%m" from
      , "fYear"      := formatTime "%Y" from
      , "tDay"       := formatTime "%d" to
      , "tMonth"     := formatTime "%m" to
      , "tYear"      := formatTime "%Y" to
      , "FileFormat" := ("EXCEL" :: String)
      ]
    access
      [ "Menu" := ("Download" :: String)
      ]
    use responseBody

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

readTime :: String -> String -> UTCTime
readTime fmt str =
    case T.parseTime defaultTimeLocale fmt str of
      Just time -> time
      Nothing   -> error (fmt ++ ": readTime: unexpected format")

formatTime :: String -> UTCTime -> String
formatTime fmt t =
    T.formatTime defaultTimeLocale fmt t

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predAct act =
    predAct >>= flip when act

--------------------------------------------------------------------------------
