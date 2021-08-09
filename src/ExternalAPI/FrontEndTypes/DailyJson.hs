{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ExternalAPI.FrontEndTypes.DailyJson where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Time (Day)
import Domain.Company (Company)
import Domain.Customer
import Domain.Daily
import GHC.Generics (Generic)
import InternalAPI.Persistence.BusinessId

calculateTotalHours :: [WorkPack] -> Double
calculateTotalHours workPacks = sum $ amount <$> workPacks

toDaily :: DailyJson -> Daily
toDaily (DailyJson i d w cId cVat alreadyInvoice _) = Daily i d w cId cVat alreadyInvoice

fromDaily :: Daily -> DailyJson
fromDaily (Daily i d w cId cVat alreadyInvoice) = DailyJson i d w cId cVat alreadyInvoice $ calculateTotalHours w

data DailyJson = DailyJson
  { id :: BusinessId Daily,
    day :: Day,
    workpacks :: [WorkPack],
    customerId :: BusinessId Customer,
    companyVat :: BusinessId Company,
    alreadyInvoice :: Bool,
    totalHours :: Double
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)
