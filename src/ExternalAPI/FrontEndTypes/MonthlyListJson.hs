{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.FrontEndTypes.MonthlyListJson where

import           Data.Aeson     (FromJSON, ToJSON)
import           Domain.Monthly (SpecificMonth (..))
import           GHC.Generics   (Generic)


data MonthlyListJson = MonthlyListJson {id :: SpecificMonth, month :: Int, year :: Integer} deriving (FromJSON, ToJSON, Generic, Ord, Eq, Show)

from :: SpecificMonth -> MonthlyListJson
from myId@(SpecificMonth y m) = MonthlyListJson  myId m (fromIntegral y)
