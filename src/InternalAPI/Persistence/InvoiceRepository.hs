{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module InternalAPI.Persistence.InvoiceRepository where

import           Control.Monad                              (void)
import           Control.Monad.IO.Class                     (MonadIO)
import           Control.Monad.Reader                       (ReaderT)
import           Data.Maybe                                 (fromJust)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Text.Internal.Builder                 (toLazyText)
import           Data.Text.Lazy                             (toStrict)
import           Data.Text.Lazy.Builder.Int                 (decimal)
import           Data.Time.Calendar                         (Day, showGregorian)
import           Data.UUID                                  (UUID)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Invoice                             (Invoice (..))
import           Domain.Monthly
import           Domain.MonthlyReport
import           InternalAPI.Persistence.BusinessId         (BusinessId (..))
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository

share
  [mkPersist sqlSettings, mkMigrate "migrateInvoice"]
  [persistLowerCase|
ReportEntryRecord
    description Text
    hours Double
    invoiceLink InvoiceRecordId
InvoiceRecord
    monthNumber Int
    year Int
    dayOfInvoice Day
    dayOfPayment Day
    customerLink CustomerRecordId
    invoiceFollowUpNumber Int
    UniqueMonth monthNumber year
    deriving Show
|]

toReportEntry :: ReportEntryRecord -> ReportEntry
toReportEntry (ReportEntryRecord d h _) = ReportEntry d h

allInvoices :: [Filter InvoiceRecord]
allInvoices = []

countInvoices :: MonadIO m => ReaderT SqlBackend m Int
countInvoices = count allInvoices

createInvoiceRecord :: CustomerRecordId -> SpecificMonth -> Int -> Day -> Day -> InvoiceRecord
createInvoiceRecord customerRecordId (SpecificMonth y m) followUpNumber today paymentDay =
  InvoiceRecord m (fromIntegral y) today paymentDay customerRecordId followUpNumber

createReportEntryRecord :: InvoiceRecordId -> ReportEntry -> ReportEntryRecord
createReportEntryRecord invoiceRecordId (ReportEntry desc h) = ReportEntryRecord desc h invoiceRecordId

insertReports :: (MonadIO m) => InvoiceRecordId -> [ReportEntry] -> ReaderT SqlBackend m [ReportEntryRecord]
insertReports invoiceRecordId entries = do
  let records = createReportEntryRecord invoiceRecordId <$> entries
  void $ insertMany records
  return records

createMonthlyReport :: InvoiceRecord -> [ReportEntryRecord] -> CustomerRecord -> MonthlyReport
createMonthlyReport (InvoiceRecord m y dayOfIn dayOfPay _ i) reportEntryRecords customerRecord =
  let totalHours = (sum $ reportEntryRecordHours <$> reportEntryRecords)
   in let totalDays = totalHours / 8
--   TODO Handle this exception better.
       in let totalExcl = totalHours * (fromJust . customerRecordHourlyRate $ customerRecord)
           in let total = 1.21 * totalExcl
               in let totalVAT = total - totalExcl
                   in MonthlyReport
                        (SpecificMonth (fromIntegral y) m)
                        totalDays
                        (toReportEntry <$> reportEntryRecords)
                        (VATReport totalExcl totalVAT total)
                        (toTextMonth m)
                        (toStrict . toLazyText . decimal $ i)
                        (Text.pack . showGregorian $ dayOfIn)
                        (Text.pack . showGregorian $ dayOfPay)

to :: [ReportEntryRecord] -> CustomerRecord -> InvoiceRecord -> Invoice
to reportEntries customerRecord invoiceRecord@(InvoiceRecord m y dayOfIn dayOfPay _ i) =
  let customer = CustomerRepository.to customerRecord
   in Invoice (SpecificMonth (toInteger y) m) (createMonthlyReport invoiceRecord reportEntries customerRecord) customer

selectMaxFollowUpNumber :: (MonadIO m) => ReaderT SqlBackend m (Maybe Int)
selectMaxFollowUpNumber = do
  maybeInvoice <- selectFirst [] [Desc InvoiceRecordInvoiceFollowUpNumber]
  return $ invoiceRecordInvoiceFollowUpNumber . entityVal <$> maybeInvoice

fetchReportEntryRecords :: MonadIO m => Entity InvoiceRecord -> ReaderT SqlBackend m [ReportEntryRecord]
fetchReportEntryRecords invoiceEntity = do
  let invoiceDBId = entityKey invoiceEntity
  reportEntities <- selectList [ReportEntryRecordInvoiceLink ==. invoiceDBId] []
  return $ entityVal <$> reportEntities

findCustomer :: (MonadIO m) => Entity InvoiceRecord -> ReaderT SqlBackend m CustomerRecord
findCustomer invoiceEntity = do
  let invoiceRecord = entityVal invoiceEntity
  maybeCustomer <- selectFirst [CustomerRecordId ==. invoiceRecordCustomerLink invoiceRecord] []
  return $ entityVal . fromJust $ maybeCustomer

fetchDependencies :: MonadIO m => Entity InvoiceRecord -> ReaderT SqlBackend m Invoice
fetchDependencies invoiceRecordEntity = do
  customerRecord <- findCustomer invoiceRecordEntity
  reportEntryRecords <- fetchReportEntryRecords invoiceRecordEntity
  return $ to reportEntryRecords customerRecord (entityVal invoiceRecordEntity)

getInvoice :: MonadIO m => SpecificMonth -> ReaderT SqlBackend m (Maybe Invoice)
getInvoice (SpecificMonth y m) = do
  maybeEntity <- getBy (UniqueMonth m (fromIntegral y))
  maybe
    (return Nothing)
    ( \entity -> do
        invoice <- fetchDependencies entity
        return $ Just invoice
    )
    maybeEntity

getInvoices :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Invoice]
getInvoices start stop = do
  records <- selectList [] [Desc InvoiceRecordYear, Desc InvoiceRecordMonthNumber, OffsetBy start, LimitTo (stop - start)]
  mapM fetchDependencies records


insertInvoice :: MonadIO m => Int -> UUID -> SpecificMonth -> [ReportEntry] -> Day -> Day -> ReaderT SqlBackend m Invoice
insertInvoice newFollowUpNumber customerId specificMonth entries today paymentDay = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId . BusinessId $ customerId
  -- TODO  No from just, fix this
  let customerRecord = fromJust maybeCustomer
  let customerRecordId = entityKey customerRecord
  let invoiceRecord = createInvoiceRecord customerRecordId specificMonth newFollowUpNumber today paymentDay
  invoiceRecordId <- insert invoiceRecord
  reportRecords <- insertReports invoiceRecordId entries
  return $ to reportRecords (entityVal customerRecord) invoiceRecord
