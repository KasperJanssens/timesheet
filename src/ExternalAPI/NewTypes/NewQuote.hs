{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewQuote where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Text            (Text)
import           Data.UUID            (UUID)
import           Domain.MonthlyReport (VATReport)
import           GHC.Generics         (Generic)

data NewQuote = NewQuote
  { total      :: Double,
    customerId :: UUID,
    companyId  :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Eq, Show)
