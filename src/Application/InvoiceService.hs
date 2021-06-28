module Application.InvoiceService where

import qualified Application.CustomerService               as CustomerService
import qualified Application.DailyService                  as DailyService
import           Application.Environment                   (AppM, poel)
import           Control.Monad.Cont                        (liftIO)
import           Control.Monad.Except                      (runExceptT)
import           Control.Monad.RWS.Class                   (asks)
import           Data.Either.Combinators                   (fromRight)
import           Data.Time
import           Data.UUID                                 (UUID)
import           Domain.Daily                              (workpacks)
import           Domain.Invoice
import           Domain.Monthly
import           Domain.MonthlyId
import           Domain.MonthlyReport
import           ExternalAPI.NewTypes.NewInvoice
import           InternalAPI.Persistence.BusinessId
import           InternalAPI.Persistence.DailyRepository   as DailyRepository
import qualified InternalAPI.Persistence.Database          as DB
import           InternalAPI.Persistence.InvoiceRepository as InvoiceRepository
import           Numeric.Natural

list :: Natural -> Natural -> AppM (Int, [Invoice])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $ do
    invoices <- InvoiceRepository.getInvoices (fromEnum from) (fromEnum to)
    amount <- InvoiceRepository.countInvoices
    return (amount, invoices)
--  return undefined

get :: BusinessId Invoice -> AppM (Maybe Invoice)
get invoiceId = do
  pool <- asks poel
  DB.executeInPool pool $ InvoiceRepository.getInvoice invoiceId

insert :: MonthlyId -> AppM Invoice
insert (MonthlyId y m customerId companyId) = do
  time <- liftIO getCurrentTime
  let today = utctDay time
  let specificMonth =  SpecificMonth y m
--  TODO fix monthly id to have explicit busoiness ids iso uuids
  paymentDay <- CustomerService.determinePaymentDate today (BusinessId customerId)
  dailies <- DailyService.getAllForMonth customerId companyId specificMonth
  let ws = concat $ workpacks <$> dailies
  let entries = createEntries ws
  pool <- asks poel
  DB.executeInPool pool $ do
    invoice <- InvoiceRepository.insertInvoice (BusinessId customerId) (BusinessId companyId) specificMonth entries today paymentDay
    DailyRepository.linkInvoiceToWorkpacks ws (Domain.Invoice.id invoice)
    DailyRepository.markAllAsInvoiced dailies
    return invoice
