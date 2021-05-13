{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module ExternalAPI.ApiType where

import qualified Data.Aeson                                as Aeson
import           Data.Text                                 (Text)
import           Data.Time                                 (Day)
import           Data.UUID                                 (UUID)
import           Domain.Customer
import           Domain.Daily
import           Domain.Invoice
import           Domain.Monthly
import           ExternalAPI.FrontEndTypes.DailyJson
import           ExternalAPI.FrontEndTypes.MonthlyListJson
import           Domain.MonthlyReport
import           ExternalAPI.NewTypes.NewCustomer
import           ExternalAPI.NewTypes.NewDaily
import           ExternalAPI.NewTypes.NewInvoice
import           ExternalAPI.WorkTypeJson
import           Numeric.Natural                           (Natural)
import           Servant

type XTotalCountHeader v = Headers '[Header "X-Total-Count" Int] v

type DailyApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [DailyJson])
    :<|> ReqBody '[JSON] NewDaily :> Post '[JSON] DailyJson
    :<|> Capture "id" DailyId :> Delete '[JSON] DailyJson
    :<|> Capture "id" DailyId :> Get '[JSON] DailyJson

type WorkTypeApi =
  Get '[JSON] (XTotalCountHeader [WorkTypeJson])

type MonthlyApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [MonthlyListJson])
--    :<|> Capture "id" SpecificMonth :> Get '[JSON] MonthlyReport

type CustomerApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [Customer])
    :<|> ReqBody '[JSON] NewCustomer :> Post '[JSON] Customer
    :<|> Capture "id" UUID :> Get '[JSON] Customer

type InvoiceApi =   QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [Invoice])
    :<|> ReqBody '[JSON] NewInvoice :> Post '[JSON] Invoice
    :<|> Capture "id" SpecificMonth :> Get '[JSON] Invoice


type WebApi =
  "v1" :> "daily" :> DailyApi
    :<|> "v1" :> "worktype" :> WorkTypeApi
    :<|> "v1" :> "monthly" :> MonthlyApi
    :<|> "v1" :> "customer" :> CustomerApi
    :<|> "v1" :> "invoice" :> InvoiceApi

api :: Proxy WebApi
api = Proxy
