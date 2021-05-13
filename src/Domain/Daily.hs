{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.Daily where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import           Data.Time.Calendar
import           Data.UUID          (UUID)
import           GHC.Generics       (Generic)

data WorkType = EDU | TEAM | MEET | INFO | TECHDESI | FUNCDESI | IMPL deriving (FromJSON, ToJSON, Generic, Enum, Bounded, Show, Eq, Read)

allWorkTypes :: [WorkType]
allWorkTypes = enumFrom minBound

data WorkPack = WorkPack
  { amount      :: Double,
    workType    :: WorkType,
    description :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)

data Daily = Daily
  { day         :: Day,
    workpacks   :: [WorkPack],
    customerId  :: UUID,
    companyName :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)
