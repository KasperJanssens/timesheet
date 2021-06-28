{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.Customer where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           InternalAPI.Persistence.BusinessId

--  TODO make this better add validations
data VATNumber = VATNumber Text deriving (FromJSON, ToJSON, Generic, Eq, Ord, Show)

--TODO no need of id, vat number should be unique enough. Fix this
data Customer = Customer
  { id            :: BusinessId Customer,
    name          :: Text,
    addressStreet :: Text,
    addressCity   :: Text,
    vatNumber     :: VATNumber,
    hourlyRate    :: Maybe Double,
    paymentTerm   :: Int
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Ord, Show)
