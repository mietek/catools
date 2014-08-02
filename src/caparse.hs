--------------------------------------------------------------------------------

module Main where

import Control.Exception (SomeException, handle)
import Control.Lens ((&), (.~), (^.))
import Data.Csv (encode)
import Data.Csv.Streaming (HasHeader (NoHeader), Records (Nil, Cons), decode)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy.Char8 as L

import Parse (parse)
import Parse.Types

import qualified Parse as P
import qualified Parse.Common as P

--------------------------------------------------------------------------------

main :: IO ()
main = do
    csv <- L.getContents
    doTxns (decode NoHeader csv)

doTxns :: Records FromTxn -> IO ()
doTxns (Cons (Right trn) more) = do
    handle report $
      L.putStr (encode ([doTxn trn]))
    doTxns more
  where
    report err =
      hPutStrLn stderr (show (err :: SomeException))
doTxns (Cons (Left err) more) = do
    hPutStrLn stderr ("doTxns: " ++ err)
    doTxns more
doTxns (Nil (Just err) rest) = do
    hPutStrLn stderr ("doTxns: " ++ err)
    L.putStrLn rest
doTxns (Nil Nothing rest) =
    L.putStrLn rest

--------------------------------------------------------------------------------

doTxn :: FromTxn -> ToTxn
doTxn trn =
    case parse P.reference (trn ^. fromReference) of
      (typ, Right det) ->
        doDetailTxn trn typ det
      (typ, Left str) ->
        doOtherTxn trn typ str

doDetailTxn :: FromTxn -> TxnType -> TxnDetail -> ToTxn
doDetailTxn trn typ det =
    emptyToTxn
      & toDate             .~ date
      & toOriginalDate     .~ origDate
      & toType             .~ show typ
      & toParty            .~ filter (/= ',') (det ^. detailParty)
      & toReference        .~ ref
      & toTerritory        .~ filter (/= ',') (det ^. detailTerritory)
      & toOriginalAmount   .~ origAmt
      & toOriginalCurrency .~ det ^. detailCurrency
      & toAmount           .~ trn ^. fromAmount
      & toBalance          .~ trn ^. fromBalance
  where
    date     = show (parse P.shortDate (trn ^. fromDate))
    origDate = maybe date show (det ^. detailDate)
    ref      = detRef ++ if null detRef || null refRef then "" else " " ++ refRef
    detRef   = det ^. detailReference
    refRef   = trn ^. fromDetail
    detAmt   = det ^. detailAmount
    origAmt  = if detAmt /= 0 then show detAmt else ""

doOtherTxn :: FromTxn -> TxnType -> String -> ToTxn
doOtherTxn trn typ str =
    emptyToTxn
      & toDate         .~ date
      & toOriginalDate .~ date
      & toType         .~ show typ
      & toParty        .~ filter (/= ',') str
      & toReference    .~ filter (/= ',') (trn ^. fromDetail)
      & toAmount       .~ trn ^. fromAmount
      & toBalance      .~ trn ^. fromBalance
  where
    date = show (parse P.shortDate (trn ^. fromDate))

--------------------------------------------------------------------------------
