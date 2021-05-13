{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.FrontEndTypes.MonthlyListJson where

import           Data.Aeson      (FromJSON, ToJSON)
import           Domain.Company
import           Domain.Customer
import           Domain.Monthly  (SpecificMonth (..))
import           GHC.Generics    (Generic)
import Application.MonthlyService (UninvoicedWork(..))


data MonthlyListJson = MonthlyListJson
  { id       :: SpecificMonth,
    month    :: Int,
    year     :: Integer,
    company  :: Company,
    customer :: Customer
  }
  deriving (FromJSON, ToJSON, Generic, Ord, Eq, Show)

from :: UninvoicedWork -> MonthlyListJson
from (UninvoicedWork myId@(SpecificMonth y m) customer company) = MonthlyListJson myId m (fromIntegral y)  company customer
