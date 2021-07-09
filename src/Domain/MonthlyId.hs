{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.MonthlyId where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.UUID                          (UUID)
import           Domain.Company                     (Company)
import           Domain.Customer                    (Customer)
import           GHC.Generics
import           InternalAPI.Persistence.BusinessId (BusinessId)

data MonthlyId = MonthlyId {year :: Int, month :: Int, customerId :: BusinessId Customer, companyId :: BusinessId Company} deriving (FromJSON, ToJSON, Generic, Show, Eq, Read, Ord)
