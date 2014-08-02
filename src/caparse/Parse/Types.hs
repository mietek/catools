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

data SrcTxn = SrcTxn
    { _srcDate         :: !String
    , _srcDetail       :: !String
    , _srcSubReference :: !String
    , _srcAmount       :: !String
    , _srcBalance      :: !String
    }
  deriving (Generic, Show)

makeLenses ''SrcTxn

instance FromRecord SrcTxn

emptySrcTxn :: SrcTxn
emptySrcTxn =
    SrcTxn
      { _srcDate         = ""
      , _srcDetail       = ""
      , _srcSubReference = ""
      , _srcAmount       = ""
      , _srcBalance      = ""
      }

--------------------------------------------------------------------------------

type TxnTypeAndDetail = (TxnType, Either String TxnDetail)

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

data DstTxn = DstTxn
    { _dstDate             :: !String
    , _dstOriginalDate     :: !String
    , _dstType             :: !String
    , _dstParty            :: !String
    , _dstReference        :: !String
    , _dstTerritory        :: !String
    , _dstOriginalAmount   :: !String
    , _dstOriginalCurrency :: !String
    , _dstAmount           :: !String
    , _dstBalance          :: !String
    }
  deriving (Generic, Show)

makeLenses ''DstTxn

instance ToRecord DstTxn

emptyDstTxn :: DstTxn
emptyDstTxn =
    DstTxn
      { _dstDate             = ""
      , _dstOriginalDate     = ""
      , _dstType             = ""
      , _dstParty            = ""
      , _dstReference        = ""
      , _dstTerritory        = ""
      , _dstOriginalAmount   = ""
      , _dstOriginalCurrency = ""
      , _dstAmount           = ""
      , _dstBalance          = ""
      }

--------------------------------------------------------------------------------
