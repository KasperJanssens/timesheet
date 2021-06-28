{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Domain.Daily where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Text                          (Text)
import           Data.Time.Calendar
import           Data.UUID                          (UUID)
import           Domain.Customer                    (Customer)
import           GHC.Generics                       (Generic)
import           InternalAPI.Persistence.BusinessId

data WorkType = EDU | TEAM | MEET | INFO | TECHDESI | FUNCDESI | IMPL deriving (FromJSON, ToJSON, Generic, Enum, Bounded, Show, Eq, Read)

allWorkTypes :: [WorkType]
allWorkTypes = enumFrom minBound

data WorkPack = WorkPack
  { wpid        :: BusinessId WorkPack,
    amount      :: Double,
    workType    :: WorkType,
    description :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)

data Daily = Daily
  { id              :: BusinessId Daily,
    day             :: Day,
    workpacks       :: [WorkPack],
    customerId      :: BusinessId Customer,
    companyVat      :: Text,
    alreadyInvoiced :: Bool
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)
