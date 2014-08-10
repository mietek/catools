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
    | RejectDirectDebit
    | RecurringDebit
    | ServiceDebit
    | PenaltyDebit
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
    show RejectDirectDebit = "reject direct debit"
    show RecurringDebit    = "recurring debit"
    show ServiceDebit      = "service debit"
    show PenaltyDebit      = "penalty debit"

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
    { _detailParty            :: !String
    , _detailCode             :: !String
    , _detailReference        :: !String
    , _detailTerritory        :: !String
    , _detailOriginalAmount   :: !Decimal
    , _detailOriginalCurrency :: !String
    , _detailOriginalDate     :: !(Maybe Day)
    , _detailConversionRate   :: !Decimal
    , _detailConversionFee    :: !Decimal
    }
  deriving (Show)

makeLenses ''TxnDetail

emptyTxnDetail :: TxnDetail
emptyTxnDetail =
    TxnDetail
      { _detailParty            = ""
      , _detailCode             = ""
      , _detailReference        = ""
      , _detailTerritory        = ""
      , _detailOriginalAmount   = 0
      , _detailOriginalCurrency = ""
      , _detailOriginalDate     = Nothing
      , _detailConversionRate   = 0
      , _detailConversionFee    = 0
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
    , _dstConversionRate   :: !String
    , _dstConversionFee    :: !String
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
      , _dstConversionRate   = ""
      , _dstConversionFee    = ""
      , _dstAmount           = ""
      , _dstBalance          = ""
      }

--------------------------------------------------------------------------------
