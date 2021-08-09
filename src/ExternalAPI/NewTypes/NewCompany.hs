{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalAPI.NewTypes.NewCompany where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Domain.VAT(VAT(..))

data NewCompany = NewCompany
  { name :: Text,
    vatNumber :: VAT,
    addressStreet :: Text,
    addressCity :: Text,
    bankAccountNumber :: Text,
    lastInvoiceNumber :: Maybe Int,
    lastQuoteNumber :: Maybe Int
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Ord, Show)

dummy :: NewCompany
dummy = NewCompany "Propellant" (BE 7673970 88) "OGS 354" "9000 Gent" "Ievrs op een bank" Nothing Nothing
