{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewFixedPriceInvoice where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Data.UUID    (UUID)
import           GHC.Generics (Generic)

data NewFixedPriceInvoice = NewFixedPriceInvoice
  { total      :: Double,
    customerId :: UUID,
    companyId :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
