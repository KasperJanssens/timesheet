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
import qualified Domain.Company                                      as Company
import qualified Domain.Customer                                     as Customer
import           Domain.FixedPriceInvoice
import qualified Domain.FixedPriceInvoice                            as FixedPriceInvoice
import           Domain.MonthlyReport
import qualified Domain.MonthlyReport                                as VATReport
import           Domain.Quote
import qualified Domain.Quote                                        as Quote
import           ExternalAPI.NewTypes.NewFixedPriceInvoice
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

get :: UUID -> AppM (Maybe FixedPriceInvoice)
get specificUUID = do
  pool <- asks poel
  DB.executeInPool pool $ FixedPriceInvoiceRepository.getFixedPriceInvoice specificUUID

insert' :: Maybe UUID -> Double -> Text -> UUID -> UUID -> AppM FixedPriceInvoice
insert' maybeQuoteId total description customerId companyId = do
  maybeCustomer <- CustomerService.get customerId
  time <- liftIO getCurrentTime
  pool <- asks poel
  DB.executeInPool pool $ do
    let today = utctDay time
    let paymentDay = CustomerService.determinePaymentDate' today (fromJust maybeCustomer)
    invoice <- FixedPriceInvoiceRepository.insertFixedPriceInvoice customerId companyId today paymentDay description total
    maybe (return ()) (QuoteRepository.linkInvoice (FixedPriceInvoice.id invoice)) maybeQuoteId
    return invoice

insert :: NewFixedPriceInvoice -> AppM FixedPriceInvoice
insert (NewFixedPriceInvoice (Left (NonQuote total customerId companyId description))) = insert' Nothing total description customerId companyId
insert (NewFixedPriceInvoice (Right uuid)) = insertFromQuote uuid

insertFromQuote :: UUID -> AppM FixedPriceInvoice
insertFromQuote quoteId = do
  maybeQuote <- QuoteService.get quoteId
  quote <- maybe (throw (err404 {errBody = "quote not found"})) return maybeQuote
  insert' (Just quoteId) (VATReport.totalExcl $ Quote.vatReport quote) (Quote.description quote) (Customer.id $ Quote.customer quote) (Company.id $ Quote.company quote)
