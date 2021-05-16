{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.Invoice where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.UUID            (UUID)
import           Domain.Company       (Company)
import           Domain.Customer      (Customer)
import           Domain.Monthly
import           Domain.MonthlyReport (MonthlyReport)
import           GHC.Generics         (Generic)

data Invoice = Invoice
  { id            :: UUID,
    specificMonth :: SpecificMonth,
    monthlyReport :: MonthlyReport,
    customer      :: Customer,
    company       :: Company
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
