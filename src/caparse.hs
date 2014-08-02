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

doTxns :: Records SrcTxn -> IO ()
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

doTxn :: SrcTxn -> DstTxn
doTxn txn =
    case parse P.typeAndDetail (txn ^. srcDetail) of
      (typ, Left str) ->
        doSimpleTxn txn typ str
      (typ, Right det) ->
        doDetailTxn txn typ det

doSimpleTxn :: SrcTxn -> TxnType -> String -> DstTxn
doSimpleTxn txn typ str =
    emptyDstTxn
      & dstDate         .~ date
      & dstOriginalDate .~ date
      & dstType         .~ show typ
      & dstParty        .~ filter (/= ',') str
      & dstReference    .~ filter (/= ',') (txn ^. srcSubReference)
      & dstAmount       .~ txn ^. srcAmount
      & dstBalance      .~ txn ^. srcBalance
  where
    date = show (parse P.shortDate (txn ^. srcDate))

doDetailTxn :: SrcTxn -> TxnType -> TxnDetail -> DstTxn
doDetailTxn txn typ det =
    emptyDstTxn
      & dstDate             .~ date
      & dstOriginalDate     .~ maybe date show (det ^. detailDate)
      & dstType             .~ show typ
      & dstParty            .~ filter (/= ',') (det ^. detailParty)
      & dstReference        .~ ref
      & dstTerritory        .~ filter (/= ',') (det ^. detailTerritory)
      & dstOriginalAmount   .~ amt
      & dstOriginalCurrency .~ det ^. detailCurrency
      & dstAmount           .~ txn ^. srcAmount
      & dstBalance          .~ txn ^. srcBalance
  where
    date   = show (parse P.shortDate (txn ^. srcDate))
    ref    = detRef ++ if null detRef || null subRef then "" else " " ++ subRef
    detRef = det ^. detailReference
    subRef = txn ^. srcSubReference
    amt    = if detAmt /= 0 then show detAmt else ""
    detAmt = det ^. detailAmount

--------------------------------------------------------------------------------
