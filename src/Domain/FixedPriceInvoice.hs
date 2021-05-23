{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.FixedPriceInvoice where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Text            (Text)
import           Data.UUID            (UUID)
import           Domain.Company       (Company)
import           Domain.Customer      (Customer)
import           Domain.Monthly
import           Domain.MonthlyReport (MonthlyReport, VATReport)
import           GHC.Generics         (Generic)

data FixedPriceInvoice = FixedPriceInvoice
  { id           :: UUID,
    invoiceId    :: Text,
    vatReport    :: VATReport,
    customer     :: Customer,
    company      :: Company,
    dayOfInvoice :: Text,
    dayOfPayment :: Text,
    description :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
