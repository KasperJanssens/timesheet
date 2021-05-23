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

import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Reader                       (ReaderT)
import           Data.Maybe                                 (fromJust)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Time.Calendar                         (Day, showGregorian)
import           Data.UUID                                  (UUID)
import qualified Data.UUID.V4                               as UUID
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.FixedPriceInvoice                   (FixedPriceInvoice (..))
import           Domain.MonthlyReport
import           InternalAPI.Persistence.BusinessId         (BusinessId (..))
import           InternalAPI.Persistence.CompanyRepository
import qualified InternalAPI.Persistence.CompanyRepository  as CompanyRepository
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository
import qualified Common.Helper as Common

share
  [mkPersist sqlSettings, mkMigrate "migrateFixedPriceInvoice"]
  [persistLowerCase|
FixedPriceInvoiceRecord
    businessId BusinessId
    totalAmount Double
    dayOfInvoice Day
    dayOfPayment Day
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

createFixedPriceInvoiceRecord :: UUID -> CustomerRecordId -> CompanyRecordId -> Int -> Day -> Day -> Double -> FixedPriceInvoiceRecord
createFixedPriceInvoiceRecord uuid customerId companyId followUpNumber today paymentDay total =
  FixedPriceInvoiceRecord (BusinessId uuid) total today paymentDay customerId companyId followUpNumber

toFixedPriceInvoice' :: UUID -> Int -> Day -> Day -> Double -> CustomerRecord -> CompanyRecord -> FixedPriceInvoice
toFixedPriceInvoice' id invoiceId today paymentDay totalExcl customerRecord companyRecord =
  let total = totalExcl * 1.21
   in let totalVat = total - totalExcl
       in let customer = CustomerRepository.to customerRecord
           in let company = CompanyRepository.to companyRecord
               in FixedPriceInvoice
                    id
                    (Common.intToText invoiceId)
                    (VATReport totalExcl totalVat total)
                    customer
                    company
                    (Text.pack . showGregorian $ today)
                    (Text.pack . showGregorian $ paymentDay)

toFixedPriceInvoice :: FixedPriceInvoiceRecord -> CustomerRecord -> CompanyRecord -> FixedPriceInvoice
toFixedPriceInvoice (FixedPriceInvoiceRecord (BusinessId id) totalExcl today paymentDay _ _ invoiceId) = toFixedPriceInvoice' id invoiceId today paymentDay totalExcl

getFixedPriceInvoice :: MonadIO m => UUID -> ReaderT SqlBackend m (Maybe FixedPriceInvoice)
getFixedPriceInvoice uuid = do
  maybeEntity <- getBy (UniqueFixedPriceBusinessId (BusinessId uuid))
  maybe
    (return Nothing)
    ( \fixedPriceInvoiceRecordEntity -> do
        let fixedPriceInvoiceRecord = entityVal fixedPriceInvoiceRecordEntity
        maybeCustomer <- selectFirst [CustomerRecordId ==. fixedPriceInvoiceRecordCustomerLink fixedPriceInvoiceRecord] []
        maybeCompany <- selectFirst [CompanyRecordId ==. fixedPriceInvoiceRecordCompanyLink fixedPriceInvoiceRecord] []
        let customerRecord = entityVal . fromJust $ maybeCustomer
        let companyRecord = entityVal . fromJust $ maybeCompany
        return $ Just $ toFixedPriceInvoice fixedPriceInvoiceRecord customerRecord companyRecord
    )
    maybeEntity

retrieveCustomer :: MonadIO m => FixedPriceInvoiceRecord -> ReaderT SqlBackend m FixedPriceInvoice
retrieveCustomer fixedPriceInvoiceRecord@(FixedPriceInvoiceRecord _ _ _ _ customerRecordId companyRecordId _) = do
  maybeCustomer <- selectFirst [CustomerRecordId ==. customerRecordId] []
  maybeCompany <- selectFirst [CompanyRecordId ==. companyRecordId] []
  let customerRecord = entityVal . fromJust $ maybeCustomer
  let companyRecord = entityVal . fromJust $ maybeCompany
  return $ toFixedPriceInvoice fixedPriceInvoiceRecord customerRecord companyRecord

getFixedPriceInvoices :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [FixedPriceInvoice]
getFixedPriceInvoices start stop = do
  records <- selectList [] [Desc FixedPriceInvoiceRecordInvoiceFollowUpNumber, OffsetBy start, LimitTo (stop - start)]
  mapM (retrieveCustomer . entityVal) records

insertFixedPriceInvoice :: MonadIO m => UUID -> UUID -> Day -> Day -> Double -> ReaderT SqlBackend m FixedPriceInvoice
insertFixedPriceInvoice customerId companyId today paymentDay total = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId . BusinessId $ customerId
  maybeCompany <- getBy . UniqueCompanyBusinessId . BusinessId $ companyId
  newFollowUpNumber <- CompanyRepository.nextNumber companyId
  uuid <- liftIO UUID.nextRandom
  -- TODO  No from just, fix this
  let customerRecord = fromJust maybeCustomer
  let companyRecord = fromJust maybeCompany
  let customerRecordId = entityKey customerRecord
  let companyRecordId = entityKey companyRecord
  let fixedPriceRecord = createFixedPriceInvoiceRecord uuid customerRecordId companyRecordId newFollowUpNumber today paymentDay total
  _ <- insert fixedPriceRecord
  return $ toFixedPriceInvoice' uuid newFollowUpNumber today paymentDay total (entityVal customerRecord) (entityVal companyRecord)
