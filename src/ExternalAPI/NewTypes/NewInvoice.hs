{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewInvoice where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)
import           Data.UUID       (UUID)
import           Domain.Monthly
import           GHC.Generics    (Generic)

data NewInvoice = NewInvoice
  { specificMonth :: SpecificMonth,
    customerId    :: UUID,
    companyId     :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
