--------------------------------------------------------------------------------

module Main where

import Control.Exception (SomeException, handle)
import Control.Lens ((^.))
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
    doRows (decode NoHeader csv)

doRows :: Records FromTransaction -> IO ()
doRows (Cons (Right trn) more) = do
    handle report $
      L.putStr (encode ([doRow trn]))
    doRows more
  where
    report err =
      hPutStrLn stderr (show (err :: SomeException))
doRows (Cons (Left err) more) = do
    hPutStrLn stderr ("doRows: " ++ err)
    doRows more
doRows (Nil (Just err) rest) = do
    hPutStrLn stderr ("doRows: " ++ err)
    L.putStrLn rest
doRows (Nil Nothing rest) =
    L.putStrLn rest

--------------------------------------------------------------------------------

doRow :: FromTransaction -> ToTransaction
doRow trn =
    case parse P.reference (trn ^. fromReference) of
      (typ, Right det) ->
        doDetailRow trn typ det
      (typ, Left str) ->
        doOtherRow trn typ str

doDetailRow :: FromTransaction -> TransactionType -> TransactionDetail -> ToTransaction
doDetailRow trn typ det =
    ToTransaction
      { _toDate             = date
      , _toOriginalDate     = origDate
      , _toType             = show typ
      , _toParty            = filter (/= ',') (det ^. detailParty)
      , _toReference        = ref
      , _toTerritory        = filter (/= ',') (det ^. detailTerritory)
      , _toOriginalAmount   = origAmt
      , _toOriginalCurrency = det ^. detailCurrency
      , _toAmount           = trn ^. fromAmount
      , _toBalance          = trn ^. fromBalance
      }
  where
    date     = show (parse P.shortDate (trn ^. fromDate))
    origDate = maybe date show (det ^. detailDate)
    ref      = detRef ++ if null detRef || null refRef then "" else " " ++ refRef
    detRef   = det ^. detailReference
    refRef   = trn ^. fromDetail
    detAmt   = det ^. detailAmount
    origAmt  = if detAmt /= 0 then show detAmt else ""

doOtherRow :: FromTransaction -> TransactionType -> String -> ToTransaction
doOtherRow trn typ str =
    ToTransaction
      { _toDate             = date
      , _toOriginalDate     = date
      , _toType             = show typ
      , _toParty            = filter (/= ',') str
      , _toReference        = filter (/= ',') (trn ^. fromDetail)
      , _toTerritory        = ""
      , _toOriginalAmount   = ""
      , _toOriginalCurrency = ""
      , _toAmount           = trn ^. fromAmount
      , _toBalance          = trn ^. fromBalance
      }
  where
    date = show (parse P.shortDate (trn ^. fromDate))

--------------------------------------------------------------------------------
