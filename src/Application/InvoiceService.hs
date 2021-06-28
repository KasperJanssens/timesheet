module Application.InvoiceService where

import qualified Application.CustomerService               as CustomerService
import qualified Application.DailyService                  as DailyService
import           Application.Environment                   (AppM, poel)
import           Control.Monad.Cont                        (liftIO)
import           Control.Monad.RWS.Class                   (asks)
import           Data.Time
import           Data.UUID                                 (UUID)
import           Domain.Daily                              (workpacks)
import           Domain.Invoice
import           Domain.Monthly
import           Domain.MonthlyReport
import           ExternalAPI.NewTypes.NewInvoice
import           InternalAPI.Persistence.DailyRepository   as DailyRepository
import qualified InternalAPI.Persistence.Database          as DB
import           InternalAPI.Persistence.InvoiceRepository as InvoiceRepository
import           Numeric.Natural
import Domain.MonthlyId
import Control.Monad.Except (runExceptT)
import Data.Either.Combinators (fromRight)

list :: Natural -> Natural -> AppM (Int, [Invoice])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $ do
    invoices <- InvoiceRepository.getInvoices (fromEnum from) (fromEnum to)
    amount <- InvoiceRepository.countInvoices
    return (amount, invoices)

get :: UUID -> AppM (Maybe Invoice)
get invoiceId = do
  pool <- asks poel
  DB.executeInPool pool $ InvoiceRepository.getInvoice invoiceId

insert :: MonthlyId -> AppM Invoice
insert (MonthlyId y m customerId companyId) = do
  time <- liftIO getCurrentTime
  let today = utctDay time
  let specificMonth =  SpecificMonth y m
  paymentDay <- CustomerService.determinePaymentDate today customerId
  dailies <- DailyService.getAllForMonth customerId companyId specificMonth
  let ws = concat $ workpacks <$> dailies
  let entries = createEntries ws
  pool <- asks poel
  DB.executeInPool pool $ do
    invoice <- InvoiceRepository.insertInvoice customerId companyId specificMonth entries today paymentDay
    DailyRepository.linkInvoiceToWorkpacks ws (Domain.Invoice.id invoice)
    DailyRepository.markAllAsInvoiced dailies
    return invoice
