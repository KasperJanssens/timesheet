{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.MonthlyId where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.UUID    (UUID)
import           GHC.Generics

data MonthlyId = MonthlyId {year :: Int, month :: Int, customerId :: UUID, companyId :: UUID} deriving (FromJSON, ToJSON, Generic, Show, Eq, Read, Ord)
