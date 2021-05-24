{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.Quote where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Text            (Text)
import           Data.UUID            (UUID)
import           Domain.Company       (Company)
import           Domain.Customer      (Customer)
import           Domain.MonthlyReport (VATReport)
import           GHC.Generics         (Generic)

data Quote = Quote
  { id        :: UUID,
    quoteId   :: Text,
    vatReport :: VATReport,
    customer  :: Customer,
    company   :: Company,
    description :: Text,
    termsOfDelivery :: Text,
    dayOfQuote :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
