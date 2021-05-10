module Application.FixedPriceInvoiceService where

import qualified Application.CustomerService                         as CustomerService
import qualified Application.DailyService                            as DailyService
import           Application.Environment                             (AppM,
                                                                      poel)
import qualified Application.MonthlyService                          as MonthlyService
import           Control.Monad.Cont                                  (liftIO)
import           Control.Monad.RWS.Class                             (asks)
import           Data.Bits                                           (shiftR)
import           Data.Maybe                                          (fromJust)
import           Data.Time
import           Data.Time.Calendar.OrdinalDate                      (toOrdinalDate)
import           Data.UUID                                           (UUID)
import           Domain.Customer
import           Domain.Daily                                        (workpacks)
import           Domain.FixedPriceInvoice
import           Domain.Invoice
import           Domain.Monthly
import           Domain.MonthlyReport
import           ExternalAPI.NewTypes.NewFixedPriceInvoice
import qualified InternalAPI.Persistence.Database                    as DB
import           InternalAPI.Persistence.FixedPriceInvoiceRepository as FixedPriceInvoiceRepository
import           Numeric.Natural

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

insert :: NewFixedPriceInvoice -> AppM FixedPriceInvoice
insert (NewFixedPriceInvoice total customerId companyId) = do
  maybeCustomer <- CustomerService.get customerId
  time <- liftIO getCurrentTime
  pool <- asks poel
  DB.executeInPool pool $ do
    let today = utctDay time
    let paymentDay = CustomerService.determinePaymentDate' today (fromJust maybeCustomer)
    FixedPriceInvoiceRepository.insertFixedPriceInvoice customerId companyId today paymentDay total
