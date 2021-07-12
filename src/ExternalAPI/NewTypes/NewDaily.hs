{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewDaily where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Text                          (Text)
import           Data.Time                          (Day)
import           Data.UUID                          (UUID)
import           Domain.Company                     (Company)
import           Domain.Customer
import           Domain.Daily
import           GHC.Generics                       (Generic)
import           InternalAPI.Persistence.BusinessId

toWorkPack :: UUID -> NewWorkPack -> WorkPack
toWorkPack uuid (NewWorkPack a w d) = WorkPack (BusinessId uuid) a w d

data NewWorkPack = NewWorkPack
  {
    amount      :: Double,
    workType    :: WorkType,
    description :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)

data NewDaily = NewDaily
  { day        :: Day,
    workpacks  :: [NewWorkPack],
    customerId :: BusinessId Customer,
    companyVat :: BusinessId Company
  }
  deriving (FromJSON, ToJSON, Generic)
