module Application.InvoiceService where

import qualified Application.CustomerService               as CustomerService
import qualified Application.DailyService                  as DailyService
import           Application.Environment                   (AppM, poel)
import qualified Application.MonthlyService                as MonthlyService
import           Control.Monad.Cont                        (liftIO)
import           Control.Monad.RWS.Class                   (asks)
import           Data.Bits                                 (shiftR)
import           Data.Maybe                                (fromJust)
import           Data.Time
import           Data.Time.Calendar.OrdinalDate            (toOrdinalDate)
import           Domain.Customer
import           Domain.Invoice
import           Domain.Monthly
import           Domain.MonthlyReport
import           ExternalAPI.NewTypes.NewInvoice
import qualified InternalAPI.Persistence.Database          as DB
import           InternalAPI.Persistence.InvoiceRepository as InvoiceRepository
import           Numeric.Natural
import Domain.Daily (workpacks)

list :: Natural -> Natural -> AppM (Int, [Invoice])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $ do
    invoices <- InvoiceRepository.getInvoices (fromEnum from) (fromEnum to)
    amount <- InvoiceRepository.countInvoices
    return (amount, invoices)

--Number always seven digits, first 4 year, next three follow up number of invoice
upWithNumber :: UTCTime -> Maybe Int -> Int
upWithNumber today Nothing =
  let curYear = fromInteger . fst . toOrdinalDate . utctDay $ today
   in (curYear * 1000) + 1
upWithNumber today (Just cur) =
  let yearFromNumber = shiftR cur 3
   in let curYear = fromInteger . fst . toOrdinalDate . utctDay $ today
       in if curYear == yearFromNumber
            then cur + 1
            else (curYear * 1000) + 1

determinePaymentDate :: Day -> Customer -> Day
determinePaymentDate today customer = addDays (toInteger $ paymentTerm customer) today

get :: SpecificMonth -> AppM (Maybe Invoice)
get specificMonth = do
  pool <- asks poel
  DB.executeInPool pool $ InvoiceRepository.getInvoice specificMonth

insert :: NewInvoice -> AppM Invoice
insert (NewInvoice specificMonth customerId) = do
  maybeCustomer <- CustomerService.get customerId
  dailies <- DailyService.getAllForMonth specificMonth
  let ws = concat $ workpacks <$> dailies
  let entries = createEntries ws
  pool <- asks poel
  DB.executeInPool pool $ do
    maxFollowUpNumber <- InvoiceRepository.selectMaxFollowUpNumber
    time <- liftIO getCurrentTime
    let newFollowUpNumber = upWithNumber time maxFollowUpNumber
    let today = utctDay time
    let paymentDay = determinePaymentDate today (fromJust maybeCustomer)
    InvoiceRepository.insertInvoice newFollowUpNumber customerId specificMonth entries today paymentDay
