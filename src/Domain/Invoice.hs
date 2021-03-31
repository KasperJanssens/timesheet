{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.Invoice where
import           Data.Aeson      (FromJSON, ToJSON)
import           Domain.Customer (Customer)
import           Domain.Monthly
import           GHC.Generics    (Generic)
import Domain.MonthlyReport (MonthlyReport)

data Invoice = Invoice {id :: SpecificMonth, monthlyReport :: MonthlyReport, customer :: Customer}deriving (FromJSON, ToJSON, Generic,  Eq, Show)
