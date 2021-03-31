{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Customer where
import Data.Text(Text)
import Numeric.Natural (Natural)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID(UUID)
import Data.String (IsString)

--  TODO make this better add validations
data VATNumber = VATNumber Text deriving (FromJSON, ToJSON, Generic, Eq, Show)

data Customer = Customer {
  id:: UUID,
  name :: Text,
  vatNumber :: VATNumber,
  hourlyRate :: Maybe Double,
  paymentTerm :: Int
} deriving (FromJSON, ToJSON, Generic, Eq, Show)