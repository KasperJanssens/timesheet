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

import           Control.Exception.Base                              (throw)
import           Control.Monad.IO.Class                              (MonadIO,
                                                                      liftIO)
import           Control.Monad.Reader                                (ReaderT)
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
import           Domain.Customer
import           Domain.FixedPriceInvoice
import           Domain.MonthlyReport                                (VATReport (..))
import           Domain.Quote
import           InternalAPI.Persistence.BusinessId                  (BusinessId (..))
import           InternalAPI.Persistence.CompanyRepository
import qualified InternalAPI.Persistence.CompanyRepository           as CompanyRepository
import           InternalAPI.Persistence.CustomerRepository          hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository          as CustomerRepository
import           InternalAPI.Persistence.FixedPriceInvoiceRepository (FixedPriceInvoiceRecordId,
                                                                      Unique (UniqueFixedPriceBusinessId))
import           InternalAPI.Persistence.RepositoryError

share
  [mkPersist sqlSettings, mkMigrate "migrateQuote"]
  [persistLowerCase|
QuoteRecord
    businessId (BusinessId Quote)
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

toQuote' :: BusinessId Quote -> Int -> Double -> Day -> Text -> Text ->  CustomerRecord -> CompanyRecord -> Quote
toQuote' businessId invoiceId totalExcl dayOfQuote termsOfDelivery description  customerRecord companyRecord =
  let total = totalExcl * 1.21
   in let totalVat = total - totalExcl
       in let customer = CustomerRepository.to customerRecord
           in let company = CompanyRepository.to companyRecord
               in Quote
                    businessId
                    (toStrict . toLazyText . decimal $ invoiceId)
                    (VATReport totalExcl totalVat total)
                    customer
                    company
                    description
                    termsOfDelivery
                    (Text.pack . showGregorian $ dayOfQuote)

toQuote :: QuoteRecord -> CustomerRecord -> CompanyRecord -> Quote
toQuote (QuoteRecord businessId totalExcl _ _ quoteId dayOfQuote description termsOfDelivery _) = toQuote' businessId quoteId totalExcl dayOfQuote description termsOfDelivery

getQuote :: MonadIO m => BusinessId Quote -> ReaderT SqlBackend m (Maybe Quote)
getQuote businessId = do
  maybeEntity <- getBy (UniqueQuoteBusinessId  businessId)
  maybe
    (return Nothing)
    ( \quoteRecordEntity -> do
        let quoteRecord = entityVal quoteRecordEntity
        maybeCustomer <- selectFirst [CustomerRecordId ==. quoteRecordCustomerLink quoteRecord] []
        maybeCompany <- selectFirst [CompanyRecordId ==. quoteRecordCompanyLink quoteRecord] []
        customer <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomer
        company <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCompany
        let customerRecord = entityVal customer
        let companyRecord = entityVal company
        return $ Just $ toQuote quoteRecord customerRecord companyRecord
    )
    maybeEntity

retrieveCustomer :: MonadIO m => QuoteRecord -> ReaderT SqlBackend m Quote
retrieveCustomer quoteRecord@(QuoteRecord _ _ customerRecordId companyRecordId _ _ _ _ _) = do
  maybeCustomer <- selectFirst [CustomerRecordId ==. customerRecordId] []
  maybeCompany <- selectFirst [CompanyRecordId ==. companyRecordId] []
  customer <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomer
  company <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCompany
  let customerRecord = entityVal customer
  let companyRecord = entityVal company
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
insertQuote :: MonadIO m => BusinessId Customer -> Text -> Double -> Day -> Text -> Text ->  ReaderT SqlBackend m Quote
insertQuote customerId companyVat total today termsOfDelivery description  = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId  $ customerId
  maybeCompany <- getBy . UniqueCompanyVAT $ companyVat
  newFollowUpNumber <- CompanyRepository.nextQuoteNumber companyVat
  uuid <- liftIO UUID.nextRandom
  customerRecord <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomer
  companyRecord <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCompany
  let customer = entityVal customerRecord
  let company = entityVal companyRecord
  let customerRecordId = entityKey customerRecord
  let companyRecordId = entityKey companyRecord

  let quoteRecord = createQuoteRecord uuid customerRecordId companyRecordId newFollowUpNumber total today termsOfDelivery description
  _ <- insert quoteRecord
  return $ toQuote' (BusinessId uuid) newFollowUpNumber total today termsOfDelivery description customer company

linkInvoice :: MonadIO m => BusinessId FixedPriceInvoice -> BusinessId Quote -> ReaderT SqlBackend m ()
linkInvoice invoiceId quoteId = do
  maybeInvoice <- getBy (UniqueFixedPriceBusinessId  invoiceId)
  maybeQuote <- getBy (UniqueQuoteBusinessId quoteId)
  invoice <- liftIO $ maybe (throw $ RepositoryError "Invoice not found") return maybeInvoice
  quote <- liftIO $ maybe (throw $ RepositoryError "Quote not found") return maybeQuote
  let quoteId = entityKey quote
  let invoiceId = entityKey invoice
  update quoteId [QuoteRecordInvoiceLink =. Just invoiceId]
