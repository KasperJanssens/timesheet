{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Company where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Data.UUID    (UUID)
import qualified Data.UUID.V4 as UUID
import           GHC.Generics (Generic)
import InternalAPI.Persistence.BusinessId
import Domain.VAT(VAT)

data Company = Company
  { id                :: BusinessId Company,
    name              :: Text,
    vatNumber         :: VAT,
    addressStreet     :: Text,
    addressCity       :: Text,
    bankAccountNumber :: Text,
    lastInvoiceNumber :: Maybe Int,
    lastQuoteNumber   :: Maybe Int
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Ord, Show)
