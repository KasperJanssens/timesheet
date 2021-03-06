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

module InternalAPI.Persistence.CustomerRepository where

import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader               (ReaderT)
import           Data.Coerce                        (coerce)
import           Data.Text                          (Text)
import           Data.Time.Clock
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Customer
import           InternalAPI.Persistence.BusinessId

share
  [mkPersist sqlSettings, mkMigrate "migrateCustomer"]
  [persistLowerCase|
CustomerRecord
    businessId (BusinessId Customer)
    name Text
    addressStreet Text
    addressCity Text
    vatNumber Text
    hourlyRate Double Maybe
    creationDate UTCTime
    paymentTerm Int
    UniqueVAT vatNumber
    UniqueCustomerBusinessId businessId
    deriving Show
|]

from :: UTCTime -> Customer -> CustomerRecord
from now (Customer businessId name  addressStreet addressCity (VATNumber vat) hourly paymentTerm) = CustomerRecord businessId name  addressStreet addressCity vat hourly now paymentTerm

to :: CustomerRecord -> Customer
to (CustomerRecord businessId name addressStreet addressCity vatNumber hourlyRate _ paymentTerm) = Customer (coerce businessId) name   addressStreet addressCity (VATNumber vatNumber) hourlyRate paymentTerm

allCustomers :: [Filter CustomerRecord]
allCustomers = []

countCustomers :: MonadIO m => ReaderT SqlBackend m Int
countCustomers = count allCustomers

findByBusinessId :: (MonadIO m) => BusinessId Customer -> ReaderT SqlBackend m (Maybe Customer)
findByBusinessId businessId = do
  maybeCustomer <- getBy (UniqueCustomerBusinessId businessId)
  return $ fmap (to . entityVal) maybeCustomer

insertCustomer :: (MonadIO m) => Customer -> ReaderT SqlBackend m CustomerRecordId
insertCustomer customer = do
  now <- liftIO getCurrentTime
  let customerRecord = from now customer
  insert customerRecord

getCustomers :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Customer]
getCustomers start stop = do
  records <- selectList [] [Asc CustomerRecordCreationDate, OffsetBy start, LimitTo (stop - start)]
  return $ to . entityVal <$> records

getAllCustomers :: MonadIO m => ReaderT SqlBackend m [Customer]
getAllCustomers = do
  records <- selectList [] [Desc CustomerRecordCreationDate]
  return $ map (to . entityVal) records

deleteCustomer :: MonadIO m => BusinessId Customer -> ReaderT SqlBackend m ()
deleteCustomer businessId = do
  deleteBy (UniqueCustomerBusinessId businessId)
