--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse where

import Control.Lens ((&), (^.), (.~))
import Control.Applicative ((<$>))
import Data.Char (isDigit, isLetter, isPrint, isSpace)
import Data.List (dropWhileEnd)

import Text.ParserCombinators.ReadP

import Parse.Common
import Parse.Territory
import Parse.Types

--------------------------------------------------------------------------------

parse :: ReadP a -> String -> a
parse p str =
    case readP_to_S p' str of
      (res, _) : _ ->
        res
      _ ->
        error ("parse: unexpected format: " ++ show str)
  where
    p' = do
      res <- p
      eof
      return res

--------------------------------------------------------------------------------

typeAndDetail :: ReadP TxnTypeAndDetail
typeAndDetail =
    leftBiasedChoice
      [ credit
      , debit
      , visaCredit
      , visaDebit
      , chequeCredit
      , foreignCredit
      , freeForeignCredit
      , directDebit
      , recurringDebit
      , serviceDebit
      ]

credit :: ReadP TxnTypeAndDetail
credit = do
    string "Giro: "
    str <- munch1 isPrint
    return (Credit, Left str)

debit :: ReadP TxnTypeAndDetail
debit = do
    string "R/P to "
    str <- munch1 isPrint
    return (Debit, Left str)

visaCredit :: ReadP TxnTypeAndDetail
visaCredit = do
    string "Visa Sales Credit "
    det <- visaDetail
    return (VisaCredit, Right det)

visaDebit :: ReadP TxnTypeAndDetail
visaDebit = do
    string "Visa Sales "
    det <- visaDetail
    let amt = 0 - (det ^. detailOriginalAmount)
    return (VisaDebit, Right (det & detailOriginalAmount .~ amt))

chequeCredit :: ReadP TxnTypeAndDetail
chequeCredit = do
    string "Cheque Deposit"
    return (ChequeCredit, Left "")

foreignCredit :: ReadP TxnTypeAndDetail
foreignCredit = do
    string "TT b/o "
    det <- foreignCreditDetail
    return (ForeignCredit, Right det)

freeForeignCredit :: ReadP TxnTypeAndDetail
freeForeignCredit = do
    string "B/o "
    det <- foreignCreditDetail
    return (FreeForeignCredit, Right det)

directDebit :: ReadP TxnTypeAndDetail
directDebit = do
    string "DD to "
    str <- munch1 isPrint
    return (DirectDebit, Left str)

recurringDebit :: ReadP TxnTypeAndDetail
recurringDebit = do
    string "S/O to "
    str <- munch1 isPrint
    return (RecurringDebit, Left str)

serviceDebit :: ReadP TxnTypeAndDetail
serviceDebit = do
    string "Service Charge"
    return (ServiceDebit, Left "")

--------------------------------------------------------------------------------

visaDetail :: ReadP TxnDetail
visaDetail = do
    party <- trim <$> count 25 (satisfy isPrint)
    char ' '
    code <- count 4 (satisfy isDigit)
    char ' '
    ref <- trim <$> count 13 (satisfy isPrint)
    char ' '
    ter <- option "" visaTerritory
    amt <- option 0 $ do
      char ' '
      amt <- decimal
      char ' '
      return amt
    optional (char ' ')
    cur <- option "" $ do
      cur <- visaCurrency
      char ' '
      return cur
    date <- visaDate
    rate <- option 0 $ do
      string " Fx "
      rate <- decimal
      char ' '
      return rate
    fee <- option 0 $ do
      string "  Fee "
      fee <- decimal
      return fee
    optional (string "  Fee")
    return $
      emptyTxnDetail
        & detailParty            .~ party
        & detailCode             .~ code
        & detailReference        .~ ref
        & detailTerritory        .~ ter
        & detailOriginalAmount   .~ amt
        & detailOriginalCurrency .~ cur
        & detailOriginalDate     .~ Just date
        & detailConversionRate   .~ rate
        & detailConversionFee    .~ fee

visaCurrency :: ReadP String
visaCurrency =
    leftBiasedChoice
      [ "EUR" ~> "EUR"
      , "USD" ~> "USD"
      , "EU"  ~> "EUR"
      , "US"  ~> "USD"
      , count 3 (satisfy isLetter)
      , count 2 (satisfy isLetter)
      ]

--------------------------------------------------------------------------------

foreignCreditDetail :: ReadP TxnDetail
foreignCreditDetail = do
    party <- many1 (satisfy isPrint)
    skipSpaces
    amt <- decimal
    cur <- count 3 (satisfy isLetter)
    string ": Buy rate "
    rate <- decimal
    return $
      emptyTxnDetail
        & detailParty            .~ party
        & detailOriginalAmount   .~ amt
        & detailOriginalCurrency .~ cur
        & detailConversionRate   .~ rate

--------------------------------------------------------------------------------

trim :: String -> String
trim str =
    dropWhile isSpace (dropWhileEnd isSpace str)

--------------------------------------------------------------------------------
