{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.FixedPriceInvoice where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Text            (Text)
import           Data.UUID            (UUID)
import           Domain.Customer      (Customer)
import           Domain.Monthly
import           Domain.MonthlyReport (MonthlyReport, VATReport)
import           GHC.Generics         (Generic)

data FixedPriceInvoice = FixedPriceInvoice {id :: UUID, invoiceId :: Text, vatReport :: VATReport, customer :: Customer, dayOfInvoice :: Text, dayOfPayment :: Text} deriving (FromJSON, ToJSON, Generic, Eq, Show)
