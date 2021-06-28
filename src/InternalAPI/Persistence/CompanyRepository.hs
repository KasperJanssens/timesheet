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
import           Data.Bits                               (shiftR)
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
import           ExternalAPI.NewTypes.NewCompany
import           InternalAPI.Persistence.BusinessId
import           InternalAPI.Persistence.RepositoryError

share
  [mkPersist sqlSettings, mkMigrate "migrateCompany"]
  [persistLowerCase|
CompanyRecord
    businessId BusinessId
    name Text
    vatNumber Text
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
to (CompanyRecord (BusinessId uuid) n v a1 a2 b c q) = Company uuid n v a1 a2 b c q

from :: UUID -> NewCompany -> CompanyRecord
from uuid (NewCompany n v a1 a2 b c q) = CompanyRecord (BusinessId uuid) n v a1 a2 b c q

getCompanyByVat :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe Company)
getCompanyByVat vatNumber = do
  companyEntity <- getBy (UniqueCompanyVAT vatNumber)
  return $ to . entityVal <$> companyEntity

getCompany :: MonadIO m => ExternalBusinessId CompanyService -> ReaderT SqlBackend m (Maybe Company)
getCompany businessId = do
  companyEntity <- getBy (UniqueCompanyBusinessId (BusinessId businessId))
  return $ to . entityVal <$> companyEntity

getCompanies :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Company]
getCompanies start stop = do
  records <- selectList [] [Desc CompanyRecordVatNumber, OffsetBy start, LimitTo (stop - start)]
  return $ map (to . entityVal) records

getAllCompanies :: MonadIO m => ReaderT SqlBackend m [Company]
getAllCompanies = do
  records <- selectList [] [Desc CompanyRecordVatNumber]
  return $ map (to . entityVal) records

insertCompany :: MonadIO m => UUID -> NewCompany -> ReaderT SqlBackend m Company
insertCompany uuid newCompany@(NewCompany n v a1 a2 b c q) = do
  void $ insert (from uuid newCompany)
  return $ Company uuid n v a1 a2 b c q

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

nextNumber :: MonadIO m => UUID -> ReaderT SqlBackend m Int
nextNumber companyId = do
  maybeCompanyEntity <- getBy (UniqueCompanyBusinessId . BusinessId $ companyId)
  companyEntity <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompanyEntity
  time <- liftIO getCurrentTime
  let nextNumber = upWithNumber time $ companyRecordCurrentLastInvoiceFollowUpNumber $ entityVal companyEntity
  update (entityKey companyEntity) [CompanyRecordCurrentLastInvoiceFollowUpNumber =. Just nextNumber]
  return nextNumber

nextQuoteNumber :: MonadIO m => Text -> ReaderT SqlBackend m Int
nextQuoteNumber vatNumber = do
  maybeCompanyEntity <- getBy (UniqueCompanyVAT vatNumber)
  companyEntity <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompanyEntity
  time <- liftIO getCurrentTime
  let nextNumber = upWithNumber time $ companyRecordCurrentLastQuoteFollowUpNumber $ entityVal companyEntity
  update (entityKey companyEntity) [CompanyRecordCurrentLastQuoteFollowUpNumber =. Just nextNumber]
  return nextNumber
