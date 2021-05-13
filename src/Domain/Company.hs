{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Company where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Company = Company
  { name              :: Text,
    vatNumber         :: Text,
    address           :: Text,
    bankAccountNumber :: Text,
    lastInvoiceNumber :: Maybe Int,
    lastQuoteNumber   :: Maybe Int
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)

dummy :: Company
dummy = Company "Propellant" "BE0767397088" "OGS 354" "Ievrs op een bank" Nothing Nothing
