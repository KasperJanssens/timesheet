{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

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
    lastQuoteNumber :: Maybe Int
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
