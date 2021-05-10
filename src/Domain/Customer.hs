{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.Customer where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.String     (IsString)
import           Data.Text       (Text)
import           Data.UUID       (UUID)
import           GHC.Generics    (Generic)
import           Numeric.Natural (Natural)

--  TODO make this better add validations
data VATNumber = VATNumber Text deriving (FromJSON, ToJSON, Generic, Eq, Show)

data Customer = Customer
  { id          :: UUID,
    name        :: Text,
    vatNumber   :: VATNumber,
    hourlyRate  :: Maybe Double,
    paymentTerm :: Int
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
