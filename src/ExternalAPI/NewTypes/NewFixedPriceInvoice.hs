{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewFixedPriceInvoice where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Text                          (Text)
import           Data.Time.Calendar                 (Day)
import           Data.UUID                          (UUID)
import           Domain.Company
import           Domain.Customer
import           Domain.Quote
import           GHC.Generics                       (Generic)
import           InternalAPI.Persistence.BusinessId

data NonQuote = NonQuote
  { total       :: Double,
    customerId  :: BusinessId Customer,
    companyId   :: BusinessId Company,
    description :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)

data NewFixedPriceInvoice = NewFixedPriceInvoice
  { invoice :: Either NonQuote (BusinessId Quote),
    day     :: Day
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
