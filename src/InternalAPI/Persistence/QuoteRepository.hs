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

import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Reader                       (ReaderT)
import           Data.Maybe                                 (fromJust)
import           Data.Text                                  (Text)
import           Data.Text.Internal.Builder                 (toLazyText)
import           Data.Text.Lazy                             (toStrict)
import           Data.Text.Lazy.Builder.Int                 (decimal)
import           Data.UUID                                  (UUID)
import qualified Data.UUID.V4                               as UUID
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.MonthlyReport                       (VATReport (..))
import           Domain.Quote
import           InternalAPI.Persistence.BusinessId         (BusinessId (..))
import           InternalAPI.Persistence.CompanyRepository
import qualified InternalAPI.Persistence.CompanyRepository  as CompanyRepository
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository

share
  [mkPersist sqlSettings, mkMigrate "migrateQuote"]
  [persistLowerCase|
QuoteRecord
    businessId BusinessId
    totalAmount Double
    customerLink CustomerRecordId
    companyLink CompanyRecordId
    quoteFollowUpNumber Int
    UniqueBusinessId businessId
    deriving Show
|]

allQuotes :: [Filter QuoteRecord]
allQuotes = []

countQuotes :: MonadIO m => ReaderT SqlBackend m Int
countQuotes = count allQuotes

createQuoteRecord :: UUID -> CustomerRecordId -> CompanyRecordId -> Int -> Double -> QuoteRecord
createQuoteRecord uuid customerId companyId followUpNumber total =
  QuoteRecord (BusinessId uuid) total customerId companyId followUpNumber

toQuote' :: UUID -> Int -> Double -> CustomerRecord -> CompanyRecord -> Quote
toQuote' id invoiceId totalExcl customerRecord companyRecord =
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

toQuote :: QuoteRecord -> CustomerRecord -> CompanyRecord -> Quote
toQuote (QuoteRecord (BusinessId id) totalExcl _ _ quoteId) = toQuote' id quoteId totalExcl

getQuote :: MonadIO m => UUID -> ReaderT SqlBackend m (Maybe Quote)
getQuote uuid = do
  maybeEntity <- getBy (UniqueBusinessId (BusinessId uuid))
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
retrieveCustomer quoteRecord@(QuoteRecord _ _ customerRecordId companyRecordId _) = do
  maybeCustomer <- selectFirst [CustomerRecordId ==. customerRecordId] []
  maybeCompany <- selectFirst [CompanyRecordId ==. companyRecordId] []
  let customerRecord = entityVal . fromJust $ maybeCustomer
  let companyRecord = entityVal . fromJust $ maybeCompany
  return $ toQuote quoteRecord customerRecord companyRecord

getQuotes :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Quote]
getQuotes start stop = do
  records <- selectList [] [Desc QuoteRecordQuoteFollowUpNumber, OffsetBy start, LimitTo (stop - start)]
  mapM (retrieveCustomer . entityVal) records

insertQuote :: MonadIO m => UUID -> Text -> Double -> ReaderT SqlBackend m Quote
insertQuote customerId companyVat total = do
  maybeCustomer <- getBy . UniqueCustomerBusinessId . BusinessId $ customerId
  maybeCompany <- getBy . UniqueCompanyVAT $ companyVat
  newFollowUpNumber <- CompanyRepository.nextQuoteNumber companyVat
  uuid <- liftIO UUID.nextRandom
  -- TODO  No from just, fix this
  let customerRecord = fromJust maybeCustomer
  let companyRecord = fromJust maybeCompany
  let customerRecordId = entityKey customerRecord
  let companyRecordId = entityKey companyRecord
  let quoteRecord = createQuoteRecord uuid customerRecordId companyRecordId newFollowUpNumber total
  _ <- insert quoteRecord
  return $ toQuote' uuid newFollowUpNumber total (entityVal customerRecord) (entityVal companyRecord)
