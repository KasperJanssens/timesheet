module Application.InvoiceService where

import qualified Application.CustomerService               as CustomerService
import qualified Application.DailyService                  as DailyService
import           Application.Environment                   (AppM, poel)
import           Control.Monad.Cont                        (liftIO)
import           Control.Monad.RWS.Class                   (asks)
import           Data.Time
import           Domain.Daily                              (workpacks)
import           Domain.Invoice
import           Domain.Monthly
import           Domain.MonthlyReport
import           ExternalAPI.NewTypes.NewInvoice
import qualified InternalAPI.Persistence.Database          as DB
import           InternalAPI.Persistence.InvoiceRepository as InvoiceRepository
import           InternalAPI.Persistence.DailyRepository as DailyRepository
import           Numeric.Natural

list :: Natural -> Natural -> AppM (Int, [Invoice])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $ do
    invoices <- InvoiceRepository.getInvoices (fromEnum from) (fromEnum to)
    amount <- InvoiceRepository.countInvoices
    return (amount, invoices)

get :: SpecificMonth -> AppM (Maybe Invoice)
get specificMonth = do
  pool <- asks poel
  DB.executeInPool pool $ InvoiceRepository.getInvoice specificMonth

insert :: NewInvoice -> AppM Invoice
insert (NewInvoice specificMonth customerId companyId) = do
  time <- liftIO getCurrentTime
  let today = utctDay time
  paymentDay <- CustomerService.determinePaymentDate today customerId
  dailies <- DailyService.getAllForMonth customerId companyId specificMonth
  let ws = concat $ workpacks <$> dailies
  let entries = createEntries ws
  pool <- asks poel
  DB.executeInPool pool $ do
    invoice <- InvoiceRepository.insertInvoice customerId companyId specificMonth entries today paymentDay
    DailyRepository.linkInvoiceToWorkpacks ws (Domain.Invoice.id invoice)
    return invoice
