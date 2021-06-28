{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalAPI.FrontEndTypes.MonthlyListJson where

import Application.MonthlyService (UninvoicedWork (..))
import Common.Helper
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Domain.Company
import qualified Domain.Company as Company
import Domain.Customer
import qualified Domain.Customer as Customer
import Domain.Monthly (SpecificMonth (..))
import Domain.MonthlyId
import GHC.Generics (Generic)
import InternalAPI.Persistence.BusinessId

data MonthlyListJson = MonthlyListJson
  { id :: MonthlyId,
    month :: Int,
    year :: Int,
    company :: Company,
    customer :: Customer
  }
  deriving (FromJSON, ToJSON, Generic, Ord, Eq, Show)

from :: UninvoicedWork -> MonthlyListJson
from (UninvoicedWork specMonth@(SpecificMonth y m) customer company) =
  MonthlyListJson (toId specMonth customer company) m (fromIntegral y) company customer

createId :: SpecificMonth -> Customer -> Company -> Text
createId (SpecificMonth y m) (Customer (BusinessId customerId) _ _ _ _ _ _) (Company (BusinessId companyId) _ _ _ _ _ _ _) =
  let yearText = intToText (fromIntegral y)
   in let monthText = intToText m
       in let customerIdText = UUID.toText customerId
           in let companyIdText = UUID.toText companyId
               in Text.intercalate "_" [yearText, monthText, customerIdText, companyIdText]

toId :: SpecificMonth -> Customer -> Company -> MonthlyId
toId (SpecificMonth y m) customer company = MonthlyId y m (Customer.id customer) (Company.id company)

fromId :: Text -> Maybe MonthlyId
fromId textId =
  let parts = Text.splitOn "_" textId
   in do
        guardedParts <- guarded (hasSize 4) parts
        year <- textToIntegral (guardedParts !! 0)
        month <- textToIntegral (guardedParts !! 1)
        customerId <- UUID.fromText (guardedParts !! 2)
        companyId <- UUID.fromText (guardedParts !! 3)
        return $ MonthlyId year month (BusinessId customerId) (BusinessId companyId)
