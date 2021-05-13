{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalAPI.NewTypes.NewCustomer where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)
import           Domain.Customer (VATNumber (..))
import           GHC.Generics    (Generic)

data NewCustomer = NewCustomer
  { name        :: Text,
    vatNumber   :: VATNumber,
    hourlyRate  :: Maybe Double,
    paymentTerm :: Int
  }
  deriving (FromJSON, ToJSON, Generic)

dummy :: NewCustomer
dummy = NewCustomer "KrondorSoft" (VATNumber "BE0893815606") (Just 75.0) 30
