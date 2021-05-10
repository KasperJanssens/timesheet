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

import           Control.Monad                  (void)
import           Control.Monad.Reader           (MonadIO, ReaderT, liftIO)
import           Data.Bits                      (shiftR)
import           Data.Maybe                     (fromJust)
import           Data.Text                      (Text)
import           Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import           Data.Time.Clock                (UTCTime, getCurrentTime,
                                                 utctDay)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Company

share
  [mkPersist sqlSettings, mkMigrate "migrateCompany"]
  [persistLowerCase|
CompanyRecord
    name Text
    vatNumber Text
    address Text
    bankAccountNumber Text
    currentLastInvoiceFollowUpNumber Int Maybe
    currentLastQuoteFollowUpNumber Int Maybe
    UniqueCompanyVAT vatNumber
    deriving Show
|]

allCompanies :: [Filter CompanyRecord]
allCompanies = []

countCompanies :: MonadIO m => ReaderT SqlBackend m Int
countCompanies = count allCompanies

to :: CompanyRecord -> Company
to (CompanyRecord n v a b c q) = Company n v a b c q

from :: Company -> CompanyRecord
from (Company n v a b c q) = CompanyRecord n v a b c q

getCompany :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe Company)
getCompany vatNumber = do
  companyEntity <- getBy (UniqueCompanyVAT vatNumber)
  return $ to . entityVal <$> companyEntity

getCompanies :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Company]
getCompanies start stop = do
  records <- selectList [] [Desc CompanyRecordVatNumber, OffsetBy start, LimitTo (stop - start)]
  return $ map (to . entityVal) records

insertCompany :: MonadIO m => Company -> ReaderT SqlBackend m Company
insertCompany company = do
  void $ insert (from company)
  return company

upWithNumber :: UTCTime -> Maybe Int -> Int
upWithNumber today Nothing =
  let curYear = fromInteger . fst . toOrdinalDate . utctDay $ today
   in (curYear * 1000) + 1
upWithNumber today (Just cur) =
  let yearFromNumber = shiftR cur 3
   in let curYear = fromInteger . fst . toOrdinalDate . utctDay $ today
       in if curYear == yearFromNumber
            then cur + 1
            else (curYear * 1000) + 1

nextNumber :: MonadIO m => Text -> ReaderT SqlBackend m Int
nextNumber vatNumber = do
  maybeCompanyEntity <- getBy (UniqueCompanyVAT vatNumber)
  -- TODO no from just, remove this
  let companyEntity = fromJust maybeCompanyEntity
  time <- liftIO getCurrentTime
  let nextNumber = upWithNumber time $ companyRecordCurrentLastInvoiceFollowUpNumber $ entityVal companyEntity
  update (entityKey companyEntity) [CompanyRecordCurrentLastInvoiceFollowUpNumber =. Just nextNumber]
  return nextNumber

nextQuoteNumber :: MonadIO m => Text -> ReaderT SqlBackend m Int
nextQuoteNumber vatNumber = do
  maybeCompanyEntity <- getBy (UniqueCompanyVAT vatNumber)
  -- TODO no from just, remove this
  let companyEntity = fromJust maybeCompanyEntity
  time <- liftIO getCurrentTime
  let nextNumber = upWithNumber time $ companyRecordCurrentLastQuoteFollowUpNumber $ entityVal companyEntity
  update (entityKey companyEntity) [CompanyRecordCurrentLastQuoteFollowUpNumber =. Just nextNumber]
  return nextNumber
