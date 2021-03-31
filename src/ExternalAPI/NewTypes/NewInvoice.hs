{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module ExternalAPI.NewTypes.NewInvoice where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.UUID       (UUID)
import           Domain.Customer (Customer)
import           Domain.Monthly
import           GHC.Generics    (Generic)

data NewInvoice = NewInvoice {specificMonth :: SpecificMonth, customerId :: UUID} deriving (FromJSON, ToJSON, Generic,  Eq, Show)
