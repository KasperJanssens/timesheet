{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ExternalAPI.NewTypes.NewInvoice where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Domain.Monthly
import Domain.MonthlyId (MonthlyId)
import GHC.Generics (Generic)
import Data.Time (Day)

data NewInvoice = NewInvoice
  { monthlyId :: MonthlyId,
    day        :: Day
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
