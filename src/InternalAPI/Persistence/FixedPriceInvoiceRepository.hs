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

module InternalAPI.Persistence.FixedPriceInvoiceRepository where

import qualified Common.Helper                              as Common
import           Control.Exception.Base                     (throw)
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Reader                       (ReaderT)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Time.Calendar                         (Day, showGregorian)
import           Data.UUID                                  (UUID)
import qualified Data.UUID.V4                               as UUID
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Company                             (Company)
import           Domain.Customer                            (Customer)
import           Domain.FixedPriceInvoice                   (FixedPriceInvoice (..))
import           Domain.MonthlyReport
import           InternalAPI.Persistence.BusinessId         (BusinessId (..))
import           InternalAPI.Persistence.CompanyRepository
import qualified InternalAPI.Persistence.CompanyRepository  as CompanyRepository
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository
import           InternalAPI.Persistence.RepositoryError

share
  [mkPersist sqlSettings, mkMigrate "migrateFixedPriceInvoice"]
  [persistLowerCase|
FixedPriceInvoiceRecord
    businessId (BusinessId FixedPriceInvoice)
    totalAmount Double
    dayOfInvoice Day
    dayOfPayment Day
    description Text
    customerLink CustomerRecordId
    companyLink CompanyRecordId
    invoiceFollowUpNumber Int
    UniqueFixedPriceBusinessId businessId
    deriving Show
|]

allFixedPriceInvoices :: [Filter FixedPriceInvoiceRecord]
allFixedPriceInvoices = []

countFixedPriceInvoices :: MonadIO m => ReaderT SqlBackend m Int
countFixedPriceInvoices = count allFixedPriceInvoices

createFixedPriceInvoiceRecord :: UUID -> CustomerRecordId -> CompanyRecordId -> Int -> Day -> Day -> Text -> Double -> FixedPriceInvoiceRecord
createFixedPriceInvoiceRecord uuid customerId companyId followUpNumber today paymentDay description  total =
  FixedPriceInvoiceRecord (BusinessId uuid) total today paymentDay description customerId companyId followUpNumber

toFixedPriceInvoice' :: BusinessId FixedPriceInvoice -> Int -> Day -> Day -> Text -> Double -> CustomerRecord -> CompanyRecord -> FixedPriceInvoice
toFixedPriceInvoice' businessId invoiceId today paymentDay description totalExcl customerRecord companyRecord =
  let total = totalExcl * 1.21
   in let totalVat = total - totalExcl
       in let customer = CustomerRepository.to customerRecord
           in let company = CompanyRepository.to companyRecord
               in FixedPriceInvoice
                    businessId
                    (Common.intToText invoiceId)
                    (VATReport totalExcl totalVat total)
                    customer
                    company
                    (Text.pack . showGregorian $ today)
                    (Text.pack . showGregorian $ paymentDay)
                    description

toFixedPriceInvoice :: FixedPriceInvoiceRecord -> CustomerRecord -> CompanyRecord -> FixedPriceInvoice
toFixedPriceInvoice (FixedPriceInvoiceRecord businessId totalExcl today  paymentDay description _ _ invoiceId) = toFixedPriceInvoice' businessId invoiceId today paymentDay description  totalExcl

getFixedPriceInvoice :: MonadIO m => BusinessId FixedPriceInvoice -> ReaderT SqlBackend m (Maybe FixedPriceInvoice)
getFixedPriceInvoice uuid = do
  maybeEntity <- getBy (UniqueFixedPriceBusinessId uuid)
  maybe
    (return Nothing)
    ( \fixedPriceInvoiceRecordEntity -> do
        let fixedPriceInvoiceRecord = entityVal fixedPriceInvoiceRecordEntity
        maybeCustomer <- selectFirst [CustomerRecordId ==. fixedPriceInvoiceRecordCustomerLink fixedPriceInvoiceRecord] []
        maybeCompany <- selectFirst [CompanyRecordId ==. fixedPriceInvoiceRecordCompanyLink fixedPriceInvoiceRecord] []
        customerEntity <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomer
        companyEntity <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompany
        let customerRecord = entityVal customerEntity
        let companyRecord = entityVal companyEntity
        return $ Just $ toFixedPriceInvoice fixedPriceInvoiceRecord customerRecord companyRecord
    )
    maybeEntity

retrieveCustomer :: MonadIO m => FixedPriceInvoiceRecord -> ReaderT SqlBackend m FixedPriceInvoice
retrieveCustomer fixedPriceInvoiceRecord@(FixedPriceInvoiceRecord _ _ _ _ _ customerRecordId companyRecordId _) = do
  maybeCustomer <- selectFirst [CustomerRecordId ==. customerRecordId] []
  maybeCompany <- selectFirst [CompanyRecordId ==. companyRecordId] []
  customerEntity <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomer
  companyEntity <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompany
  let customerRecord = entityVal customerEntity
  let companyRecord = entityVal companyEntity
  return $ toFixedPriceInvoice fixedPriceInvoiceRecord customerRecord companyRecord

getFixedPriceInvoices :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [FixedPriceInvoice]
getFixedPriceInvoices start stop = do
  records <- selectList [] [Desc FixedPriceInvoiceRecordInvoiceFollowUpNumber, OffsetBy start, LimitTo (stop - start)]
  mapM (retrieveCustomer . entityVal) records

insertFixedPriceInvoice :: MonadIO m => BusinessId Customer -> BusinessId Company -> Day -> Day -> Text -> Double -> ReaderT SqlBackend m FixedPriceInvoice
insertFixedPriceInvoice customerId companyId today paymentDay description total  = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId  $ customerId
  maybeCompany <- getBy . UniqueCompanyBusinessId  $ companyId
  newFollowUpNumber <- CompanyRepository.nextNumber companyId
  uuid <- liftIO UUID.nextRandom
  customerRecord <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomer
  companyRecord <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompany
  let customerRecordId = entityKey customerRecord
  let companyRecordId = entityKey companyRecord
  let fixedPriceRecord = createFixedPriceInvoiceRecord uuid customerRecordId companyRecordId newFollowUpNumber today paymentDay description total
  _ <- insert fixedPriceRecord
  return $ toFixedPriceInvoice' (BusinessId uuid) newFollowUpNumber today paymentDay description total (entityVal customerRecord) (entityVal companyRecord)
