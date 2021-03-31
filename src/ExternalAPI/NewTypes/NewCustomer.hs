{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewCustomer where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)
import           Domain.Customer (VATNumber)
import           GHC.Generics    (Generic)
import           Numeric.Natural (Natural)

data NewCustomer = NewCustomer
  { name       :: Text,
    vatNumber  :: VATNumber,
    hourlyRate :: Maybe Double,
    paymentTerm :: Int
  }
  deriving (FromJSON, ToJSON, Generic)
