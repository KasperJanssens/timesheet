{-# LANGUAGE OverloadedStrings #-}

module Application.FixedPriceInvoiceService where

import qualified Application.CustomerService                         as CustomerService
import           Application.Environment                             (AppM,
                                                                      poel)
import qualified Application.QuoteService                            as QuoteService
import           Control.Exception.Base                              (throw)
import           Control.Monad.Cont                                  (liftIO)
import           Control.Monad.IO.Class                              (MonadIO)
import           Control.Monad.Reader                                (ReaderT)
import           Control.Monad.RWS.Class                             (asks)
import           Data.Either                                         (fromLeft)
import           Data.Maybe                                          (fromJust)
import           Data.Text                                           (Text)
import           Data.Time
import           Data.UUID                                           (UUID)
import           Database.Persist.Sql                                (SqlBackend)
import           Domain.Company
import qualified Domain.Company                                      as Company
import           Domain.Customer
import qualified Domain.Customer                                     as Customer
import           Domain.FixedPriceInvoice
import qualified Domain.FixedPriceInvoice                            as FixedPriceInvoice
import           Domain.MonthlyReport
import qualified Domain.MonthlyReport                                as VATReport
import           Domain.Quote
import qualified Domain.Quote                                        as Quote
import           ExternalAPI.NewTypes.NewFixedPriceInvoice
import           InternalAPI.Persistence.BusinessId
import qualified InternalAPI.Persistence.Database                    as DB
import           InternalAPI.Persistence.FixedPriceInvoiceRepository as FixedPriceInvoiceRepository
import           InternalAPI.Persistence.QuoteRepository             as QuoteRepository
import           Numeric.Natural
import           Servant.Server.Internal.ServerError                 (err404,
                                                                      errBody)

list :: Natural -> Natural -> AppM (Int, [FixedPriceInvoice])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $ do
    invoices <- FixedPriceInvoiceRepository.getFixedPriceInvoices (fromEnum from) (fromEnum to)
    amount <- FixedPriceInvoiceRepository.countFixedPriceInvoices
    return (amount, invoices)

get :: BusinessId FixedPriceInvoice -> AppM (Maybe FixedPriceInvoice)
get specificUUID = do
  pool <- asks poel
  DB.executeInPool pool $ FixedPriceInvoiceRepository.getFixedPriceInvoice specificUUID

insert' :: Maybe (BusinessId Quote) -> Double -> Text -> BusinessId Customer -> BusinessId Company -> Day -> AppM FixedPriceInvoice
insert' maybeQuoteId total description customerId companyId today = do
  maybeCustomer <- CustomerService.get customerId
  pool <- asks poel
  DB.executeInPool pool $ do
    let paymentDay = CustomerService.determinePaymentDate' today (fromJust maybeCustomer)
    invoice <- FixedPriceInvoiceRepository.insertFixedPriceInvoice customerId companyId today paymentDay description total
    maybe (return ()) (QuoteRepository.linkInvoice (FixedPriceInvoice.id invoice)) maybeQuoteId
    return invoice

insert :: NewFixedPriceInvoice -> AppM FixedPriceInvoice
insert (NewFixedPriceInvoice (Left (NonQuote total customerId companyId description)) day) = insert' Nothing total description customerId companyId day
insert (NewFixedPriceInvoice (Right quoteBusinessId) day) = insertFromQuote quoteBusinessId day

insertFromQuote :: BusinessId Quote -> Day ->  AppM FixedPriceInvoice
insertFromQuote quoteId today= do
  maybeQuote <- QuoteService.get quoteId
  quote <- maybe (throw (err404 {errBody = "quote not found"})) return maybeQuote
  insert' (Just quoteId) (VATReport.totalExcl $ Quote.vatReport quote) (Quote.description quote) (Customer.id $ Quote.customer quote) (Company.id $ Quote.company quote) today
