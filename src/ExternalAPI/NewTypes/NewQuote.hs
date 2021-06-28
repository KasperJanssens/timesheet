{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewQuote where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Text                          (Text)
import           Data.UUID                          (UUID)
import           Domain.MonthlyReport               (VATReport)
import           Domain.Customer
import           GHC.Generics                       (Generic)
import           InternalAPI.Persistence.BusinessId (BusinessId)

data NewQuote = NewQuote
  { total           :: Double,
    customerId      :: BusinessId Customer,
    companyId       :: Text,
    description     :: Text,
    termsOfDelivery :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
