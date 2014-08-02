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
doTxns (Cons (Right txn) more) = do
    handle report $
      L.putStr (encode ([doTxn txn]))
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
doTxn txn =
    case parse P.typeAndDetail (txn ^. fromDetail) of
      (typ, Right det) ->
        doDetailTxn txn typ det
      (typ, Left str) ->
        doOtherTxn txn typ str

doDetailTxn :: FromTxn -> TxnType -> TxnDetail -> ToTxn
doDetailTxn txn typ det =
    emptyToTxn
      & toDate             .~ date
      & toOriginalDate     .~ maybe date show (det ^. detailDate)
      & toType             .~ show typ
      & toParty            .~ filter (/= ',') (det ^. detailParty)
      & toReference        .~ ref
      & toTerritory        .~ filter (/= ',') (det ^. detailTerritory)
      & toOriginalAmount   .~ amt
      & toOriginalCurrency .~ det ^. detailCurrency
      & toAmount           .~ txn ^. fromAmount
      & toBalance          .~ txn ^. fromBalance
  where
    date   = show (parse P.shortDate (txn ^. fromDate))
    ref    = detRef ++ if null detRef || null txnRef then "" else " " ++ txnRef
    txnRef = txn ^. fromReference
    detRef = det ^. detailReference
    amt    = if detAmt /= 0 then show detAmt else ""
    detAmt = det ^. detailAmount

doOtherTxn :: FromTxn -> TxnType -> String -> ToTxn
doOtherTxn txn typ str =
    emptyToTxn
      & toDate         .~ date
      & toOriginalDate .~ date
      & toType         .~ show typ
      & toParty        .~ filter (/= ',') str
      & toReference    .~ filter (/= ',') (txn ^. fromReference)
      & toAmount       .~ txn ^. fromAmount
      & toBalance      .~ txn ^. fromBalance
  where
    date = show (parse P.shortDate (txn ^. fromDate))

--------------------------------------------------------------------------------
