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

data FromTxn = FromTxn
    { _fromDate      :: !String
    , _fromReference :: !String
    , _fromDetail    :: !String
    , _fromAmount    :: !String
    , _fromBalance   :: !String
    }
  deriving (Generic, Show)

makeLenses ''FromTxn

instance FromRecord FromTxn

emptyFromTxn :: FromTxn
emptyFromTxn =
    FromTxn
      { _fromDate      = ""
      , _fromReference = ""
      , _fromDetail    = ""
      , _fromAmount    = ""
      , _fromBalance   = ""
      }

--------------------------------------------------------------------------------

data ToTxn = ToTxn
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

makeLenses ''ToTxn

instance ToRecord ToTxn

emptyToTxn :: ToTxn
emptyToTxn =
    ToTxn
      { _toDate             = ""
      , _toOriginalDate     = ""
      , _toType             = ""
      , _toParty            = ""
      , _toReference        = ""
      , _toTerritory        = ""
      , _toOriginalAmount   = ""
      , _toOriginalCurrency = ""
      , _toAmount           = ""
      , _toBalance          = ""
      }

--------------------------------------------------------------------------------

data TxnType =
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

instance Show TxnType
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

--------------------------------------------------------------------------------

type TxnReference = (TxnType, Either String TxnDetail)

data TxnDetail = TxnDetail
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

makeLenses ''TxnDetail

emptyTxnDetail :: TxnDetail
emptyTxnDetail =
    TxnDetail
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
