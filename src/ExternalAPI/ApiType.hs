{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExternalAPI.ApiType where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (Day)
import Data.UUID (UUID)
import Domain.Company
import Domain.Customer
import Domain.Daily
import Domain.ExternalBusinessId
import Domain.FixedPriceInvoice
import Domain.Invoice
import Domain.Monthly
import Domain.MonthlyId
import Domain.MonthlyReport
import Domain.Quote
import ExternalAPI.FrontEndTypes.DailyJson
import ExternalAPI.FrontEndTypes.MonthlyListJson
import ExternalAPI.NewTypes.NewCompany
import ExternalAPI.NewTypes.NewCustomer
import ExternalAPI.NewTypes.NewDaily
import ExternalAPI.NewTypes.NewFixedPriceInvoice
import ExternalAPI.NewTypes.NewInvoice
import ExternalAPI.NewTypes.NewQuote
import ExternalAPI.WorkTypeJson
import Numeric.Natural (Natural)
import Servant
import InternalAPI.Persistence.BusinessId

type XTotalCountHeader v = Headers '[Header "X-Total-Count" Int] v

data QuoteFilter = Uninvoiced deriving (Read)

instance ToHttpApiData QuoteFilter where
  toUrlPiece Uninvoiced = "uninvoiced"

instance FromHttpApiData QuoteFilter where
  parseUrlPiece "uninvoiced" = Right Uninvoiced
  parseUrlPiece _ = Left "did not recognize fermentor filter"

type DailyApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [DailyJson])
    :<|> ReqBody '[JSON] NewDaily :> Post '[JSON] DailyJson
    :<|> Capture "id" (BusinessId Daily) :> Delete '[JSON] DailyJson
    :<|> Capture "id" (BusinessId Daily) :> Get '[JSON] DailyJson

type WorkTypeApi =
  Get '[JSON] (XTotalCountHeader [WorkTypeJson])

type MonthlyApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [MonthlyListJson])

type CustomerApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [Customer])
    :<|> ReqBody '[JSON] NewCustomer :> Post '[JSON] Customer
    :<|> Capture "id" (BusinessId Customer) :> Get '[JSON] Customer

type CompanyApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [Company])
    :<|> ReqBody '[JSON] NewCompany :> Post '[JSON] Company
    :<|> Capture "id" (BusinessId Company) :> Get '[JSON] Company

type InvoiceApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [Invoice])
    :<|> ReqBody '[JSON] NewInvoice :> Post '[JSON] Invoice
    :<|> Capture "id" (BusinessId Invoice) :> Get '[JSON] Invoice

type QuoteApi =
  QueryParam "filter" QuoteFilter :> QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [Quote])
    :<|> ReqBody '[JSON] NewQuote :> Post '[JSON] Quote
    :<|> Capture "id" (BusinessId Quote) :> Get '[JSON] Quote

type FixedPriceInvoiceApi =
  QueryParam "_start" Natural :> QueryParam "_end" Natural :> Get '[JSON] (XTotalCountHeader [FixedPriceInvoice])
    :<|> ReqBody '[JSON] NewFixedPriceInvoice :> Post '[JSON] FixedPriceInvoice
    :<|> Capture "id" (BusinessId FixedPriceInvoice) :> Get '[JSON] FixedPriceInvoice

type WebApi =
  "v1" :> "daily" :> DailyApi
    :<|> "v1" :> "worktype" :> WorkTypeApi
    :<|> "v1" :> "monthly" :> MonthlyApi
    :<|> "v1" :> "customer" :> CustomerApi
    :<|> "v1" :> "invoice" :> InvoiceApi
    :<|> "v1" :> "company" :> CompanyApi
    :<|> "v1" :> "quote" :> QuoteApi
    :<|> "v1" :> "fixedPriceInvoice" :> FixedPriceInvoiceApi

api :: Proxy WebApi
api = Proxy
