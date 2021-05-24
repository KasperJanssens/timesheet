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

module InternalAPI.Persistence.QuoteRepository where

import           Control.Monad.IO.Class                              (MonadIO,
                                                                      liftIO)
import           Control.Monad.Reader                                (ReaderT)
import           Data.Maybe                                          (fromJust)
import           Data.Text                                           (Text)
import qualified Data.Text                                           as Text
import           Data.Text.Internal.Builder                          (toLazyText)
import           Data.Text.Lazy                                      (toStrict)
import           Data.Text.Lazy.Builder.Int                          (decimal)
import           Data.Time.Calendar                                  (Day, showGregorian)
import           Data.UUID                                           (UUID)
import qualified Data.UUID.V4                                        as UUID
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.MonthlyReport                                (VATReport (..))
import           Domain.Quote
import           InternalAPI.Persistence.BusinessId                  (BusinessId (..))
import           InternalAPI.Persistence.CompanyRepository
import qualified InternalAPI.Persistence.CompanyRepository           as CompanyRepository
import           InternalAPI.Persistence.CustomerRepository          hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository          as CustomerRepository
import           InternalAPI.Persistence.FixedPriceInvoiceRepository (FixedPriceInvoiceRecordId,
                                                                      Unique (UniqueFixedPriceBusinessId))

share
  [mkPersist sqlSettings, mkMigrate "migrateQuote"]
  [persistLowerCase|
QuoteRecord
    businessId BusinessId
    totalAmount Double
    customerLink CustomerRecordId
    companyLink CompanyRecordId
    quoteFollowUpNumber Int
    dayOfQuote Day
    description Text
    termsOfDelivery Text
    UniqueQuoteBusinessId businessId
    invoiceLink FixedPriceInvoiceRecordId Maybe
    deriving Show
|]

allQuotes :: [Filter QuoteRecord]
allQuotes = []

countQuotes :: MonadIO m => ReaderT SqlBackend m Int
countQuotes = count allQuotes

createQuoteRecord :: UUID -> CustomerRecordId -> CompanyRecordId -> Int -> Double -> Day -> Text ->  Text -> QuoteRecord
createQuoteRecord uuid customerId companyId followUpNumber total dayOfQuote termsOfDelivery description =
  QuoteRecord (BusinessId uuid) total customerId companyId followUpNumber dayOfQuote description termsOfDelivery Nothing

toQuote' :: UUID -> Int -> Double -> Day -> Text -> Text ->  CustomerRecord -> CompanyRecord -> Quote
toQuote' id invoiceId totalExcl dayOfQuote termsOfDelivery description  customerRecord companyRecord =
  let total = totalExcl * 1.21
   in let totalVat = total - totalExcl
       in let customer = CustomerRepository.to customerRecord
           in let company = CompanyRepository.to companyRecord
               in Quote
                    id
                    (toStrict . toLazyText . decimal $ invoiceId)
                    (VATReport totalExcl totalVat total)
                    customer
                    company
                    description
                    termsOfDelivery
                    (Text.pack . showGregorian $ dayOfQuote)

toQuote :: QuoteRecord -> CustomerRecord -> CompanyRecord -> Quote
toQuote (QuoteRecord (BusinessId id) totalExcl _ _ quoteId dayOfQuote description termsOfDelivery _) = toQuote' id quoteId totalExcl dayOfQuote description termsOfDelivery

getQuote :: MonadIO m => UUID -> ReaderT SqlBackend m (Maybe Quote)
getQuote uuid = do
  maybeEntity <- getBy (UniqueQuoteBusinessId (BusinessId uuid))
  maybe
    (return Nothing)
    ( \quoteRecordEntity -> do
        let quoteRecord = entityVal quoteRecordEntity
        maybeCustomer <- selectFirst [CustomerRecordId ==. quoteRecordCustomerLink quoteRecord] []
        maybeCompany <- selectFirst [CompanyRecordId ==. quoteRecordCompanyLink quoteRecord] []
        let customerRecord = entityVal . fromJust $ maybeCustomer
        let companyRecord = entityVal . fromJust $ maybeCompany
        return $ Just $ toQuote quoteRecord customerRecord companyRecord
    )
    maybeEntity

retrieveCustomer :: MonadIO m => QuoteRecord -> ReaderT SqlBackend m Quote
retrieveCustomer quoteRecord@(QuoteRecord _ _ customerRecordId companyRecordId _ _ _ _ _) = do
  maybeCustomer <- selectFirst [CustomerRecordId ==. customerRecordId] []
  maybeCompany <- selectFirst [CompanyRecordId ==. companyRecordId] []
  let customerRecord = entityVal . fromJust $ maybeCustomer
  let companyRecord = entityVal . fromJust $ maybeCompany
  return $ toQuote quoteRecord customerRecord companyRecord

getQuotes :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Quote]
getQuotes start stop = do
  records <- selectList [] [Desc QuoteRecordQuoteFollowUpNumber, OffsetBy start, LimitTo (stop - start)]
  mapM (retrieveCustomer . entityVal) records

listNonInvoiced :: MonadIO m => ReaderT SqlBackend m [Quote]
listNonInvoiced = do
  records <- selectList [QuoteRecordInvoiceLink ==. Nothing] []
  mapM (retrieveCustomer . entityVal) records

--TODO companyId instead of companyVat
insertQuote :: MonadIO m => UUID -> Text -> Double -> Day -> Text -> Text ->  ReaderT SqlBackend m Quote
insertQuote customerId companyVat total today termsOfDelivery description  = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId . BusinessId $ customerId
  maybeCompany <- getBy . UniqueCompanyVAT $ companyVat
  newFollowUpNumber <- CompanyRepository.nextQuoteNumber companyVat
  uuid <- liftIO UUID.nextRandom
  -- TODO  No from just, fix this
  let customerRecord = fromJust maybeCustomer
  let companyRecord = fromJust maybeCompany
  let customerRecordId = entityKey customerRecord
  let companyRecordId = entityKey companyRecord
  
  let quoteRecord = createQuoteRecord uuid customerRecordId companyRecordId newFollowUpNumber total today termsOfDelivery description
  _ <- insert quoteRecord
  return $ toQuote' uuid newFollowUpNumber total today termsOfDelivery description (entityVal customerRecord) (entityVal companyRecord)

linkInvoice :: MonadIO m => UUID -> UUID -> ReaderT SqlBackend m ()
linkInvoice invoiceId quoteId = do
  maybeInvoice <- getBy (UniqueFixedPriceBusinessId (BusinessId invoiceId))
  maybeQuote <- getBy (UniqueQuoteBusinessId (BusinessId quoteId))
  let quoteId = entityKey $ fromJust maybeQuote
  let invoiceId = entityKey $ fromJust maybeInvoice
  update quoteId [QuoteRecordInvoiceLink =. Just invoiceId]
