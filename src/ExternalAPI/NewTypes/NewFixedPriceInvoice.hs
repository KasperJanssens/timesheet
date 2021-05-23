{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewFixedPriceInvoice where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Data.UUID    (UUID)
import           GHC.Generics (Generic)

data NonQuote = NonQuote
  { total      :: Double,
    customerId :: UUID,
    companyId  :: UUID,
    description :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)

data NewFixedPriceInvoice = NewFixedPriceInvoice
  { invoice :: Either NonQuote UUID
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
