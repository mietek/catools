--------------------------------------------------------------------------------

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
        error ("parse: unexpected format: " ++ str)
  where
    p' = do
      res <- p
      eof
      return res

--------------------------------------------------------------------------------

typeAndDetail :: ReadP TxnTypeAndDetail
typeAndDetail =
    choice
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
    skipString "Giro: "
    str <- munch1 isPrint
    return (Credit, Left str)

debit :: ReadP TxnTypeAndDetail
debit = do
    skipString "R/P to "
    str <- munch1 isPrint
    return (Debit, Left str)

visaCredit :: ReadP TxnTypeAndDetail
visaCredit = do
    skipString "Visa Sales Credit "
    det <- visaDetail
    return (VisaCredit, Right det)

visaDebit :: ReadP TxnTypeAndDetail
visaDebit = do
    skipString "Visa Sales "
    det <- visaDetail
    let amt = 0 - (det ^. detailAmount)
    return (VisaDebit, Right (det & detailAmount .~ amt))

chequeCredit :: ReadP TxnTypeAndDetail
chequeCredit = do
    skipString "Cheque Deposit"
    return (ChequeCredit, Left "")

foreignCredit :: ReadP TxnTypeAndDetail
foreignCredit = do
    skipString "TT b/o "
    det <- foreignCreditDetail
    return (ForeignCredit, Right det)

freeForeignCredit :: ReadP TxnTypeAndDetail
freeForeignCredit = do
    skipString "B/o "
    det <- foreignCreditDetail
    return (FreeForeignCredit, Right det)

directDebit :: ReadP TxnTypeAndDetail
directDebit = do
    skipString "DD to "
    str <- munch1 isPrint
    return (DirectDebit, Left str)

recurringDebit :: ReadP TxnTypeAndDetail
recurringDebit = do
    skipString "S/O to "
    str <- munch1 isPrint
    return (RecurringDebit, Left str)

serviceDebit :: ReadP TxnTypeAndDetail
serviceDebit = do
    skipString "Service Charge"
    return (ServiceDebit, Left "")

--------------------------------------------------------------------------------

visaDetail :: ReadP TxnDetail
visaDetail = do
    party <- trim <$> count 25 (satisfy isPrint)
    skipSpace
    code <- count 4 (satisfy isDigit)
    skipSpace
    ref <- trim <$> count 13 (satisfy isPrint)
    skipSpace
    ter <- option "" visaTerritory
    amt <- option 0 $ do
      skipSpace
      amt <- decimal
      skipSpace
      return amt
    optional skipSpace
    cur <- option "" $ do
      cur <- visaCurrency
      skipSpace
      return cur
    date <- visaDate
    (rate, fee) <- option (0, 0) $ do
      skipString " Fx "
      rate <- decimal
      fee <- option 0 $ do
        skipString "   Fee "
        decimal
      return (rate, fee)
    _ <- munch isPrint
    return $
      emptyTxnDetail
        & detailParty     .~ party
        & detailCode      .~ code
        & detailReference .~ ref
        & detailTerritory .~ ter
        & detailAmount    .~ amt
        & detailCurrency  .~ cur
        & detailDate      .~ Just date
        & detailRate      .~ rate
        & detailFee       .~ fee

visaCurrency :: ReadP String
visaCurrency =
    choice
      [ istring "EU"  >> return "EUR"
      , istring "US"  >> return "USD"
      , istring "USD" >> return "USD"
      , many1 (satisfy isLetter)
      ]

--------------------------------------------------------------------------------

foreignCreditDetail :: ReadP TxnDetail
foreignCreditDetail = do
    party <- many1 (satisfy isPrint)
    skipSpaces
    amt <- decimal
    cur <- count 3 (satisfy isLetter)
    skipString ": Buy rate "
    rate <- decimal
    return $
      emptyTxnDetail
        & detailParty    .~ party
        & detailAmount   .~ amt
        & detailCurrency .~ cur
        & detailRate     .~ rate

--------------------------------------------------------------------------------

trim :: String -> String
trim str =
    dropWhile isSpace (dropWhileEnd isSpace str)

--------------------------------------------------------------------------------
