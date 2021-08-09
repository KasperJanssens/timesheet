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

module InternalAPI.Persistence.CompanyRepository where

import           Control.Exception.Base                  (throw)
import           Control.Monad                           (void)
import           Control.Monad.Reader                    (MonadIO, ReaderT,
                                                          liftIO)
import           Data.Text                               (Text)
import           Data.Time.Calendar.OrdinalDate          (toOrdinalDate)
import           Data.Time.Clock                         (UTCTime,
                                                          getCurrentTime,
                                                          utctDay)
import           Data.UUID                               (UUID)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Company
import           Domain.ExternalBusinessId
import           Domain.VAT                              (VAT)
import           ExternalAPI.NewTypes.NewCompany
import           InternalAPI.Persistence.BusinessId
import           InternalAPI.Persistence.RepositoryError

share
  [mkPersist sqlSettings, mkMigrate "migrateCompany"]
  [persistLowerCase|
CompanyRecord
    businessId (BusinessId Company)
    name Text
    vatNumber VAT
    addressStreet Text
    addressCity Text
    bankAccountNumber Text
    currentLastInvoiceFollowUpNumber Int Maybe
    currentLastQuoteFollowUpNumber Int Maybe
    UniqueCompanyVAT vatNumber
    UniqueCompanyBusinessId businessId
    deriving Show
|]

allCompanies :: [Filter CompanyRecord]
allCompanies = []

countCompanies :: MonadIO m => ReaderT SqlBackend m Int
countCompanies = count allCompanies

to :: CompanyRecord -> Company
to (CompanyRecord businessId n v a1 a2 b c q) = Company businessId n v a1 a2 b c q

from :: UUID -> NewCompany -> CompanyRecord
from uuid (NewCompany n v a1 a2 b c q) = CompanyRecord (BusinessId uuid) n v a1 a2 b c q

getCompanyByVat :: MonadIO m => VAT -> ReaderT SqlBackend m (Maybe Company)
getCompanyByVat vatNumber = do
  companyEntity <- getBy (UniqueCompanyVAT vatNumber)
  return $ to . entityVal <$> companyEntity

getCompany :: MonadIO m => BusinessId Company -> ReaderT SqlBackend m (Maybe Company)
getCompany businessId = do
  companyEntity <- getBy (UniqueCompanyBusinessId businessId)
  return $ to . entityVal <$> companyEntity

getCompanies :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Company]
getCompanies start stop = do
  records <- selectList [] [Desc CompanyRecordVatNumber, OffsetBy start, LimitTo (stop - start)]
  return $ map (to . entityVal) records

getAllCompanies :: MonadIO m => ReaderT SqlBackend m [Company]
getAllCompanies = do
  records <- selectList [] [Desc CompanyRecordVatNumber]
  return $ map (to . entityVal) records

insertCompany :: MonadIO m => BusinessId Company -> NewCompany -> ReaderT SqlBackend m Company
insertCompany businessId@(BusinessId uuid) newCompany@(NewCompany n v a1 a2 b c q) = do
  void $ insert (from uuid newCompany)
  return $ Company businessId n v a1 a2 b c q

upWithNumber :: UTCTime -> Maybe Int -> Int
upWithNumber today Nothing =
  let curYear = fromInteger . fst . toOrdinalDate . utctDay $ today
   in (curYear * 1000) + 1
upWithNumber today (Just cur) =
  let yearFromNumber = cur `div` 1000
   in let curYear = fromInteger . fst . toOrdinalDate . utctDay $ today
       in if curYear == yearFromNumber
            then cur + 1
            else (curYear * 1000) + 1

nextNumber :: MonadIO m => BusinessId Company -> ReaderT SqlBackend m Int
nextNumber companyBusinessId@(BusinessId companyId) = do
  maybeCompanyEntity <- getBy (UniqueCompanyBusinessId companyBusinessId)
  companyEntity <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompanyEntity
  time <- liftIO getCurrentTime
  let nextNumber = upWithNumber time $ companyRecordCurrentLastInvoiceFollowUpNumber $ entityVal companyEntity
  update (entityKey companyEntity) [CompanyRecordCurrentLastInvoiceFollowUpNumber =. Just nextNumber]
  return nextNumber

nextQuoteNumber :: MonadIO m => BusinessId Company -> ReaderT SqlBackend m Int
nextQuoteNumber companyId = do
  maybeCompanyEntity <- getBy (UniqueCompanyBusinessId companyId)
  companyEntity <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompanyEntity
  time <- liftIO getCurrentTime
  let nextNumber = upWithNumber time $ companyRecordCurrentLastQuoteFollowUpNumber $ entityVal companyEntity
  update (entityKey companyEntity) [CompanyRecordCurrentLastQuoteFollowUpNumber =. Just nextNumber]
  return nextNumber
