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
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
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
import           InternalAPI.Persistence.CompanyRepository hiding (to)
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository
import qualified InternalAPI.Persistence.CompanyRepository as CompanyRepository
import qualified Data.UUID.V4 as UUID

share
  [mkPersist sqlSettings, mkMigrate "migrateInvoice"]
  [persistLowerCase|
ReportEntryRecord
    description Text
    hours Double
    invoiceLink InvoiceRecordId
InvoiceRecord
    businessId BusinessId
    monthNumber Int
    year Int
    dayOfInvoice Day
    dayOfPayment Day
    customerLink CustomerRecordId
    companyLink CompanyRecordId
    invoiceFollowUpNumber Int
    UniqueMonth monthNumber year customerLink companyLink
    UniqueInvoiceBusinessId businessId
    deriving Show
|]

toReportEntry :: Double -> ReportEntryRecord -> ReportEntry
toReportEntry rate (ReportEntryRecord d h _) = ReportEntry d h (rate * h)

allInvoices :: [Filter InvoiceRecord]
allInvoices = []

countInvoices :: MonadIO m => ReaderT SqlBackend m Int
countInvoices = count allInvoices

createInvoiceRecord :: UUID -> CustomerRecordId -> CompanyRecordId -> SpecificMonth -> Int -> Day -> Day -> InvoiceRecord
createInvoiceRecord businessId customerRecordId companyRecordId (SpecificMonth y m) followUpNumber today paymentDay =
  InvoiceRecord (BusinessId businessId) m (fromIntegral y) today paymentDay customerRecordId companyRecordId followUpNumber

createReportEntryRecord :: InvoiceRecordId -> ReportEntry -> ReportEntryRecord
createReportEntryRecord invoiceRecordId (ReportEntry desc h _) = ReportEntryRecord desc h invoiceRecordId

insertReports :: (MonadIO m) => InvoiceRecordId -> [ReportEntry] -> ReaderT SqlBackend m [ReportEntryRecord]
insertReports invoiceRecordId entries = do
  let records = createReportEntryRecord invoiceRecordId <$> entries
  void $ insertMany records
  return records

createMonthlyReport :: InvoiceRecord -> [ReportEntryRecord] -> CustomerRecord -> MonthlyReport
createMonthlyReport (InvoiceRecord _ m y dayOfIn dayOfPay _ _ i) reportEntryRecords customerRecord =
  let totalHours = (sum $ reportEntryRecordHours <$> reportEntryRecords)
   in let totalDays = totalHours / 8
       in --   TODO Handle this exception better.
         let hourlyRate = (fromJust . customerRecordHourlyRate $ customerRecord) in
          let totalExcl = totalHours * hourlyRate
           in let total = 1.21 * totalExcl
               in let totalVAT = total - totalExcl
                   in MonthlyReport
                        (SpecificMonth (fromIntegral y) m)
                        totalDays
                        (toReportEntry hourlyRate <$> reportEntryRecords)
                        (VATReport totalExcl totalVAT total)
                        (toTextMonth m)
                        (toStrict . toLazyText . decimal $ i)
                        (Text.pack . showGregorian $ dayOfIn)
                        (Text.pack . showGregorian $ dayOfPay)

to :: [ReportEntryRecord] -> CustomerRecord -> CompanyRecord -> InvoiceRecord -> Invoice
to reportEntries customerRecord companyRecord invoiceRecord@(InvoiceRecord (BusinessId businessId) m y dayOfIn dayOfPay _ _ i) =
  let customer = CustomerRepository.to customerRecord in
  let company = CompanyRepository.to companyRecord 
   in Invoice businessId (SpecificMonth y m) (createMonthlyReport invoiceRecord reportEntries customerRecord) customer company

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

findCompany :: (MonadIO m) => Entity InvoiceRecord -> ReaderT SqlBackend m CompanyRecord
findCompany invoiceEntity = do
  let invoiceRecord = entityVal invoiceEntity
  maybeCompany <- selectFirst [CompanyRecordId ==. invoiceRecordCompanyLink invoiceRecord] []
  return $ entityVal . fromJust $ maybeCompany

fetchDependencies :: MonadIO m => Entity InvoiceRecord -> ReaderT SqlBackend m Invoice
fetchDependencies invoiceRecordEntity = do
  customerRecord <- findCustomer invoiceRecordEntity
  companyRecord <- findCompany invoiceRecordEntity
  reportEntryRecords <- fetchReportEntryRecords invoiceRecordEntity
  return $ to reportEntryRecords customerRecord companyRecord (entityVal invoiceRecordEntity)

getInvoice :: MonadIO m => UUID -> ReaderT SqlBackend m (Maybe Invoice)
getInvoice invoiceId = do
  maybeEntity <- getBy (UniqueInvoiceBusinessId (BusinessId invoiceId))
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

insertInvoice :: MonadIO m =>  UUID -> UUID ->  SpecificMonth -> [ReportEntry] -> Day -> Day -> ReaderT SqlBackend m Invoice
insertInvoice  customerId companyId specificMonth entries today paymentDay = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId . BusinessId $ customerId
  maybeCompany <- getBy . UniqueCompanyBusinessId . BusinessId $ companyId
  newFollowUpNumber <- CompanyRepository.nextNumber companyId
  -- TODO  No from just, fix this
  let customerRecord = fromJust maybeCustomer
  let companyRecord = fromJust maybeCompany
  let customerRecordId = entityKey customerRecord
  let companyRecordId = entityKey companyRecord
  uuid <- liftIO UUID.nextRandom
  let invoiceRecord = createInvoiceRecord uuid customerRecordId companyRecordId specificMonth newFollowUpNumber today paymentDay
  invoiceRecordId <- insert invoiceRecord
  reportRecords <- insertReports invoiceRecordId entries
  return $ to reportRecords (entityVal customerRecord) (entityVal companyRecord) invoiceRecord
