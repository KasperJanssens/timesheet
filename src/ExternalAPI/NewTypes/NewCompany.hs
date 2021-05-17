{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalAPI.NewTypes.NewCompany where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data NewCompany = NewCompany
  { name              :: Text,
    vatNumber         :: Text,
    address           :: Text,
    bankAccountNumber :: Text,
    lastInvoiceNumber :: Maybe Int,
    lastQuoteNumber   :: Maybe Int
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Ord, Show)

dummy :: NewCompany
dummy = NewCompany "Propellant" "BE0767397088" "OGS 354" "Ievrs op een bank" Nothing Nothing
