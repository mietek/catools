--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, liftIO)
import Control.Lens ((.=), (^.), (<~), makeLenses, set, use)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isDigit)
import Data.Decimal (Decimal)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, parseTime)
import Network.HTTP.Client (CookieJar, createCookieJar)
import Network.Wreq (FormParam ((:=)))
import System.Environment (getArgs, getEnv, getProgName)
import System.Locale (defaultTimeLocale)
import Text.HTML.TagSoup ((~/=), fromAttrib, fromTagText, parseTags)

import qualified Data.ByteString.Lazy.Char8 as Lbs
import qualified Network.Wreq as Wreq
import qualified Text.HTML.TagSoup as Soup

--------------------------------------------------------------------------------

type Tag = Soup.Tag ByteString

data SessionState = SessionState
    { _cookieJar      :: !CookieJar
    , _responseBody   :: !ByteString
    , _responseTags   :: ![Tag]
    , _responseToken  :: !ByteString
    , _accountOwner   :: !ByteString
    , _accountNumber  :: !ByteString
    , _accountName    :: !ByteString
    , _accountBalance :: !Decimal
    }

makeLenses ''SessionState

type Session a = StateT SessionState IO a

--------------------------------------------------------------------------------

main :: IO ()
main = do
    getArgs >>= \case
      [fromDate, toDate] -> do
        user <- getEnv "CA_USERNAME"
        code <- getEnv "CA_ACCESS_CODE"
        pass <- getEnv "CA_PASSWORD"
        flip evalStateT emptySession $ do
          login user code pass
          owner   <- use accountOwner
          number  <- use accountNumber
          name    <- use accountName
          balance <- use accountBalance
          csv     <- getTransactionsAsCsv fromDate toDate
          liftIO $ do
            putStrLn ("Account owner:   " ++ Lbs.unpack owner)
            putStrLn ("Account number:  " ++ Lbs.unpack number)
            putStrLn ("Account name:    " ++ Lbs.unpack name)
            putStrLn ("Account balance: " ++ show balance)
            Lbs.putStrLn csv
          logout
      _ -> do
        name <- getProgName
        putStrLn ("Usage: " ++ name ++ " YYYY-MM-DD YYYY-MM-DD")

--------------------------------------------------------------------------------

baseUrl :: String
baseUrl = "https://www.caterallenonline.co.uk"

mainUrl :: String
mainUrl = baseUrl ++ "/WebAccess.dll"

login :: String -> String -> String -> Session ()
login user code pass = do
    _ <- liftIO (Wreq.get baseUrl)
    postWithSession mainUrl []
    (c1, c2, c3) <- prepareSecret codeChallenge code
    postWithSession mainUrl
      [ "UserID" := user
      , "pwd1"   := c1
      , "pwd2"   := c2
      , "pwd3"   := c3
      ]
    (p1, p2, p3) <- prepareSecret passChallenge pass
    postWithSession mainUrl
      [ "pwd1" := p1
      , "pwd2" := p2
      , "pwd3" := p3
      ]
    whenM (isJust <$> tryScrapeTag continueTag) $
      postWithSession mainUrl
        [ "Menu" := ("Continue" :: String)
        ]
    accountOwner   <~ scrapeTagTextAfter 3 ownerTag
    accountNumber  <~ fromAttrib "value" <$> scrapeTag accountTag
    accountName    <~ scrapeTagTextAfter 8 accountTag
    accountBalance <~ acceptDecimal <$> scrapeTagTextAfter 12 accountTag
  where
    continueTag     = "<input type=submit name=Menu value=Continue>"
    ownerTag        = "<div id=name_right_header_border"
    accountTag      = "<input type=submit name=Acc>"
    codeChallenge n = "<label for=ipos" ++ show n ++ ">"
    passChallenge n = "<label for=pos"  ++ show n ++ ">"

logout :: Session ()
logout = do
    postWithSession mainUrl
      [ "Menu" := ("Log Out" :: String)
      ]

getTransactionsAsCsv :: String -> String -> Session ByteString
getTransactionsAsCsv fromDate toDate = do
    accNum <- use accountNumber
    postWithSession mainUrl
      [ "Acc" := accNum
      ]
    postWithSession mainUrl
      [ "Menu" := ("Download Transaction Details" :: String)
      ]
    postWithSession mainUrl
      [ "Menu"       := ("Accept" :: String)
      , "fDay"       := fd
      , "fMonth"     := fm
      , "fYear"      := fy
      , "tDay"       := td
      , "tMonth"     := tm
      , "tYear"      := ty
      , "FileFormat" := ("EXCEL" :: String)
      ]
    postWithSession mainUrl
      [ "Menu" := ("Download" :: String)
      ]
    use responseBody
  where
    (fy, fm, fd) = splitIsoDate fromDate
    (ty, tm, td) = splitIsoDate toDate

--------------------------------------------------------------------------------

emptySession :: SessionState
emptySession =
    SessionState
      { _cookieJar = createCookieJar []
      , _responseBody   = Lbs.empty
      , _responseTags   = []
      , _responseToken  = Lbs.empty
      , _accountOwner   = Lbs.empty
      , _accountNumber  = Lbs.empty
      , _accountName    = Lbs.empty
      , _accountBalance = 0
      }

postWithSession :: String -> [FormParam] -> Session ()
postWithSession url params = do
    jar   <- use cookieJar
    token <- use responseToken
    let opts = set Wreq.cookies jar Wreq.defaults
    response <- liftIO (Wreq.postWith opts url (("Trxn" := token) : params))
    cookieJar    .= response ^. Wreq.responseCookieJar
    responseBody .= response ^. Wreq.responseBody
    responseTags .= parseTags (response ^. Wreq.responseBody)
    tryScrapeTag tokenTag >>= \case
      Just tag -> responseToken .= fromAttrib "value" tag
      Nothing  -> responseToken .= Lbs.empty
  where
    tokenTag = "<input type=hidden name=Trxn>"

prepareSecret :: (Int -> String) -> String -> Session (String, String, String)
prepareSecret challengeTag secret = do
    i1 <- readIndex <$> scrapeTagTextAfter 1 (challengeTag 1)
    i2 <- readIndex <$> scrapeTagTextAfter 1 (challengeTag 2)
    i3 <- readIndex <$> scrapeTagTextAfter 1 (challengeTag 3)
    return ([secret !! i1], [secret !! i2], [secret !! i3])

scrapeTag :: String -> Session Tag
scrapeTag like =
    fromJust <$> tryScrapeTag like

scrapeTagTextAfter :: Int -> String -> Session ByteString
scrapeTagTextAfter n like =
    fromTagText <$> scrapeTagAfter n like

scrapeTagAfter :: Int -> String -> Session Tag
scrapeTagAfter n like =
    fromJust <$> tryScrapeTagAfter n like

tryScrapeTag :: String -> Session (Maybe Tag)
tryScrapeTag like =
    tryScrapeTagAfter 0 like

tryScrapeTagAfter :: Int -> String -> Session (Maybe Tag)
tryScrapeTagAfter n like = do
    tags <- use responseTags
    return (listToMaybe (drop n (dropWhile (~/= like) tags)))

--------------------------------------------------------------------------------

splitIsoDate :: String -> (String, String, String)
splitIsoDate s =
    let date = fromJust (parse "%Y-%m-%d" s) :: UTCTime in
    (format "%Y" date, format "%m" date, format "%d" date)
  where
    parse  = parseTime defaultTimeLocale
    format = formatTime defaultTimeLocale

acceptDecimal :: ByteString -> Decimal
acceptDecimal s =
    read (filter (\c -> isDigit c || c == '.') (Lbs.unpack s))

readIndex :: ByteString -> Int
readIndex s =
    read (Lbs.unpack s) - 1

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predAct act =
    predAct >>= flip when act

--------------------------------------------------------------------------------
