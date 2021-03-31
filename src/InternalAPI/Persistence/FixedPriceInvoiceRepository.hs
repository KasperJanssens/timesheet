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
import qualified Data.Text                                  as Text
import           Data.Text.Internal.Builder                 (toLazyText)
import           Data.Text.Lazy                             (toStrict)
import           Data.Text.Lazy.Builder.Int                 (decimal)
import           Data.Time.Calendar                         (Day, showGregorian)
import           Data.UUID                                  (UUID)
import qualified Data.UUID.V4                               as UUID
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Daily                               (Daily)
import           Domain.FixedPriceInvoice                   (FixedPriceInvoice (..))
import           Domain.Invoice                             (Invoice (..))
import           Domain.Monthly
import           Domain.MonthlyReport
import           InternalAPI.Persistence.BusinessId         (BusinessId (..))
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository

share
  [mkPersist sqlSettings, mkMigrate "migrateFixedPriceInvoice"]
  [persistLowerCase|
FixedPriceInvoiceRecord
    businessId BusinessId
    totalAmount Double
    dayOfInvoice Day
    dayOfPayment Day
    customerLink CustomerRecordId
    invoiceFollowUpNumber Int
    UniqueBusinessId businessId
    deriving Show
|]

allFixedPriceInvoices :: [Filter FixedPriceInvoiceRecord]
allFixedPriceInvoices = []

createFixedPriceInvoiceRecord :: UUID -> CustomerRecordId -> Int -> Day -> Day -> Double -> FixedPriceInvoiceRecord
createFixedPriceInvoiceRecord uuid customerId followUpNumber today paymentDay total =
  FixedPriceInvoiceRecord (BusinessId uuid) total today paymentDay customerId followUpNumber

toFixedPrice :: UUID -> Int -> Day -> Day -> Double -> CustomerRecord -> FixedPriceInvoice
toFixedPrice id invoiceId today paymentDay totalExcl customerRecord =
  let total = totalExcl * 1.21
   in let totalVat = total - totalExcl
       in let customer = CustomerRepository.to customerRecord
           in FixedPriceInvoice id (toStrict . toLazyText . decimal $ invoiceId) (VATReport totalExcl totalVat total) customer (Text.pack . showGregorian $ today) (Text.pack . showGregorian $ paymentDay)

to :: FixedPriceInvoiceRecord -> CustomerRecord -> FixedPriceInvoice
to (FixedPriceInvoiceRecord (BusinessId id) totalExcl today paymentDay _ invoiceId) = toFixedPrice id invoiceId today paymentDay totalExcl

getFixedPriceInvoice :: MonadIO m => UUID -> ReaderT SqlBackend m (Maybe FixedPriceInvoice)
getFixedPriceInvoice uuid = do
  maybeEntity <- getBy (UniqueBusinessId (BusinessId uuid))
  maybe
    (return Nothing)
    ( \fixedPriceInvoiceRecordEntity -> do
        let fixedPriceInvoiceRecord = entityVal fixedPriceInvoiceRecordEntity
        maybeCustomer <- selectFirst [CustomerRecordId ==. fixedPriceInvoiceRecordCustomerLink fixedPriceInvoiceRecord] []
        let customerRecord = entityVal . fromJust $ maybeCustomer
        return $ Just $ to fixedPriceInvoiceRecord customerRecord
    )
    maybeEntity

retrieveCustomer :: MonadIO m => FixedPriceInvoiceRecord -> ReaderT SqlBackend m FixedPriceInvoice
retrieveCustomer fixedPriceInvoiceRecord@(FixedPriceInvoiceRecord _ _ _ _ customerRecordId _) = do
  maybeCustomer <- selectFirst [CustomerRecordId ==. customerRecordId] []
  let customerRecord = entityVal . fromJust $ maybeCustomer
  return $ to fixedPriceInvoiceRecord customerRecord

getFixedPriceInvoices :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [FixedPriceInvoice]
getFixedPriceInvoices start stop = do
  records <- selectList [] [Desc FixedPriceInvoiceRecordInvoiceFollowUpNumber, OffsetBy start, LimitTo (stop - start)]
  mapM (retrieveCustomer . entityVal) records

insertFixedPriceInvoice :: MonadIO m => Int -> UUID -> Day -> Day -> Double -> ReaderT SqlBackend m FixedPriceInvoice
insertFixedPriceInvoice newFollowUpNumber customerId today paymentDay total = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId . BusinessId $ customerId
  uuid <- liftIO UUID.nextRandom
  -- TODO  No from just, fix this
  let customerRecord = fromJust maybeCustomer
  let customerRecordId = entityKey customerRecord
  let fixedPriceRecord = createFixedPriceInvoiceRecord uuid customerRecordId newFollowUpNumber today paymentDay total
  _ <- insert fixedPriceRecord
  return $ toFixedPrice uuid newFollowUpNumber today paymentDay total (entityVal customerRecord)
