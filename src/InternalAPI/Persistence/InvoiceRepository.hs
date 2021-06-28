{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
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

import           Control.Exception.Base                     (IOException (..),
                                                             throw)
import           Control.Monad                              (void)
import           Control.Monad.Error.Class                  (MonadError)
import           Control.Monad.Except                       (ExceptT)
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Reader                       (ReaderT)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Text.Internal.Builder                 (toLazyText)
import           Data.Text.Lazy                             (toStrict)
import           Data.Text.Lazy.Builder.Int                 (decimal)
import           Data.Time.Calendar                         (Day, showGregorian)
import           Data.UUID                                  (UUID)
import qualified Data.UUID.V4                               as UUID
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Invoice                             (Invoice (..))
import           Domain.Monthly
import           Domain.MonthlyReport
import           GHC.IO.Exception                           (userError)
import           InternalAPI.Persistence.BusinessId         (BusinessId (..))
import           InternalAPI.Persistence.CompanyRepository  hiding (to)
import qualified InternalAPI.Persistence.CompanyRepository  as CompanyRepository
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository
import           InternalAPI.Persistence.RepositoryError    (RepositoryError (..))

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

createMonthlyReport :: (MonadIO m) => InvoiceRecord -> [ReportEntryRecord] -> CustomerRecord -> m MonthlyReport
createMonthlyReport (InvoiceRecord _ m y dayOfIn dayOfPay _ _ i) reportEntryRecords customerRecord =
  let totalHours = (sum $ reportEntryRecordHours <$> reportEntryRecords)
   in let totalDays = totalHours / 8
       in do
          hourlyRate <- maybe (throw $ RepositoryError "Customer has no hourly rate associated with it") return (customerRecordHourlyRate customerRecord)
          let totalExcl = totalHours * hourlyRate
          let total = 1.21 * totalExcl
          let totalVAT = total - totalExcl
          return $ MonthlyReport
                            (SpecificMonth (fromIntegral y) m)
                            totalDays
                            (toReportEntry hourlyRate <$> reportEntryRecords)
                            (VATReport totalExcl totalVAT total)
                            (toTextMonth m)
                            (toStrict . toLazyText . decimal $ i)
                            (Text.pack . showGregorian $ dayOfIn)
                            (Text.pack . showGregorian $ dayOfPay)

to :: (MonadIO m) => [ReportEntryRecord] -> CustomerRecord -> CompanyRecord -> InvoiceRecord -> m Invoice
to reportEntries customerRecord companyRecord invoiceRecord@(InvoiceRecord (BusinessId businessId) m y dayOfIn dayOfPay _ _ i) =
  let customer = CustomerRepository.to customerRecord
   in let company = CompanyRepository.to companyRecord in
     do
       monthlyReport <- createMonthlyReport invoiceRecord reportEntries customerRecord
       return $ Invoice businessId (SpecificMonth y m) monthlyReport customer company

selectMaxFollowUpNumber :: (MonadIO m) => ReaderT SqlBackend m (Maybe Int)
selectMaxFollowUpNumber = do
  maybeInvoice <- selectFirst [] [Desc InvoiceRecordInvoiceFollowUpNumber]
  return $ invoiceRecordInvoiceFollowUpNumber . entityVal <$> maybeInvoice

fetchReportEntryRecords :: MonadIO m => Entity InvoiceRecord -> ReaderT SqlBackend m [ReportEntryRecord]
fetchReportEntryRecords invoiceEntity = do
  let invoiceDBId = entityKey invoiceEntity
  reportEntities <- selectList [ReportEntryRecordInvoiceLink ==. invoiceDBId] []
  return $ entityVal <$> reportEntities

findCustomer :: (MonadIO m) => Entity InvoiceRecord -> ReaderT SqlBackend m (Maybe CustomerRecord)
findCustomer invoiceEntity = do
  let invoiceRecord = entityVal invoiceEntity
  maybeCustomer <- selectFirst [CustomerRecordId ==. invoiceRecordCustomerLink invoiceRecord] []
  return $ entityVal <$> maybeCustomer

findCompany :: (MonadIO m) => Entity InvoiceRecord -> ReaderT SqlBackend m (Maybe CompanyRecord)
findCompany invoiceEntity = do
  let invoiceRecord = entityVal invoiceEntity
  maybeCompany <- selectFirst [CompanyRecordId ==. invoiceRecordCompanyLink invoiceRecord] []
  return $ entityVal <$> maybeCompany

fetchDependencies :: MonadIO m => Entity InvoiceRecord -> ReaderT SqlBackend m Invoice
fetchDependencies invoiceRecordEntity = do
  maybeCustomerRecord <- findCustomer invoiceRecordEntity
  maybeCompanyRecord <- findCompany invoiceRecordEntity
  reportEntryRecords <- fetchReportEntryRecords invoiceRecordEntity
  customerRecord <- liftIO $ maybe (throw $ RepositoryError "Could not retrieve customer dependency of invoice") return maybeCustomerRecord
  companyRecord <- liftIO $ maybe (throw $ RepositoryError "Could not retrieve company dependency of invoice") return maybeCompanyRecord
  to reportEntryRecords customerRecord companyRecord (entityVal invoiceRecordEntity)

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

getInvoices :: (MonadIO m) => Int -> Int -> ReaderT SqlBackend m [Invoice]
getInvoices start stop = do
  records <- selectList [] [Desc InvoiceRecordYear, Desc InvoiceRecordMonthNumber, OffsetBy start, LimitTo (stop - start)]
  mapM fetchDependencies records

insertInvoice :: MonadIO m => UUID -> UUID -> SpecificMonth -> [ReportEntry] -> Day -> Day -> ReaderT SqlBackend m Invoice
insertInvoice customerId companyId specificMonth entries today paymentDay = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId . BusinessId $ customerId
  maybeCompany <- getBy . UniqueCompanyBusinessId . BusinessId $ companyId
  newFollowUpNumber <- CompanyRepository.nextNumber companyId
  customerRecord <- maybe (throw (RepositoryError "Customer not found, wrong id")) return maybeCustomer
  companyRecord <- maybe (throw (RepositoryError "Company not found, wrong id")) return maybeCompany
  let customerRecordId = entityKey customerRecord
  let companyRecordId = entityKey companyRecord
  uuid <- liftIO UUID.nextRandom
  let invoiceRecord = createInvoiceRecord uuid customerRecordId companyRecordId specificMonth newFollowUpNumber today paymentDay
  invoiceRecordId <- insert invoiceRecord
  reportRecords <- insertReports invoiceRecordId entries
  to reportRecords (entityVal customerRecord) (entityVal companyRecord) invoiceRecord
