--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Parse.Types where

import Control.Lens (makeLenses)
import Data.Csv (FromRecord, ToRecord)
import Data.Decimal (Decimal)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data FromTransaction = FromTransaction
    { _fromDate      :: !String
    , _fromReference :: !String
    , _fromDetail    :: !String
    , _fromAmount    :: !String
    , _fromBalance   :: !String
    }
  deriving (Generic, Show)

instance FromRecord FromTransaction

makeLenses ''FromTransaction

--------------------------------------------------------------------------------

data ToTransaction = ToTransaction
    { _toDate             :: !String
    , _toOriginalDate     :: !String
    , _toType             :: !String
    , _toParty            :: !String
    , _toReference        :: !String
    , _toTerritory        :: !String
    , _toOriginalAmount   :: !String
    , _toOriginalCurrency :: !String
    , _toAmount           :: !String
    , _toBalance          :: !String
    }
  deriving (Generic, Show)

instance ToRecord ToTransaction

makeLenses ''ToTransaction

--------------------------------------------------------------------------------

type TransactionReference = (TransactionType, Either String TransactionDetail)

data TransactionType =
      Credit
    | Debit
    | VisaCredit
    | VisaDebit
    | ChequeCredit
    | ForeignCredit
    | FreeForeignCredit
    | DirectDebit
    | RecurringDebit
    | ServiceDebit
  deriving (Eq)

data TransactionDetail = TransactionDetail
    { _detailParty     :: !String
    , _detailCode      :: !String
    , _detailReference :: !String
    , _detailTerritory :: !String
    , _detailAmount    :: !Decimal
    , _detailCurrency  :: !String
    , _detailDate      :: !(Maybe Day)
    , _detailRate      :: !Decimal
    , _detailFee       :: !Decimal
    }
  deriving (Show)

instance Show TransactionType
  where
    show Credit            = "credit"
    show Debit             = "debit"
    show VisaCredit        = "Visa credit"
    show VisaDebit         = "Visa debit"
    show ChequeCredit      = "cheque credit"
    show ForeignCredit     = "foreign credit"
    show FreeForeignCredit = "free foreign credit"
    show DirectDebit       = "direct debit"
    show RecurringDebit    = "recurring debit"
    show ServiceDebit      = "service debit"

makeLenses ''TransactionDetail

emptyDetail :: TransactionDetail
emptyDetail =
    TransactionDetail
      { _detailParty     = ""
      , _detailCode      = ""
      , _detailReference = ""
      , _detailTerritory = ""
      , _detailAmount    = 0
      , _detailCurrency  = ""
      , _detailDate      = Nothing
      , _detailRate      = 0
      , _detailFee       = 0
      }

--------------------------------------------------------------------------------
