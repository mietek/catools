--------------------------------------------------------------------------------

module Main where

import Control.Exception (SomeException, handle)
import Control.Lens ((&), (.~), (^.), ASetter)
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
      & dstDate         .! date
      & dstOriginalDate .! date
      & dstType         .! show typ
      & dstParty        .! str
      & dstReference    .! txn ^. srcSubReference
      & dstAmount       .! txn ^. srcAmount
      & dstBalance      .! txn ^. srcBalance
  where
    date = show (parse P.shortDate (txn ^. srcDate))

doDetailTxn :: SrcTxn -> TxnType -> TxnDetail -> DstTxn
doDetailTxn txn typ det =
    emptyDstTxn
      & dstDate             .! date
      & dstOriginalDate     .! maybe date show (det ^. detailOriginalDate)
      & dstType             .! show typ
      & dstParty            .! det ^. detailParty
      & dstReference        .! det ^. detailReference ++ txn ^. srcSubReference
      & dstTerritory        .! det ^. detailTerritory
      & dstOriginalAmount   .! showNotZero (det ^. detailOriginalAmount)
      & dstOriginalCurrency .! det ^. detailOriginalCurrency
      & dstConversionRate   .! showNotZero (det ^. detailConversionRate)
      & dstConversionFee    .! showNotZero (det ^. detailConversionFee)
      & dstAmount           .! txn ^. srcAmount
      & dstBalance          .! txn ^. srcBalance
  where
    date = show (parse P.shortDate (txn ^. srcDate))
    showNotZero n = if n /= 0 then show n else ""

--------------------------------------------------------------------------------

infixr 2 .!

(.!) :: ASetter s t a String -> [Char] -> s -> t
x .! y =
  x .~ P.trim (filter (/= ',') y)

--------------------------------------------------------------------------------
