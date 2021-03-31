{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module InternalAPI.Persistence.Database where

import           Control.Monad                              (when)
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Logger                       (NoLoggingT,
                                                             runStdoutLoggingT)
import           Control.Monad.Logger.CallStack             (runStderrLoggingT)
import           Control.Monad.Reader                       (ReaderT,
                                                             runReaderT)
import           Control.Monad.Trans.Resource               (ResourceT)
import           Data.ByteString.Char8                      (ByteString)
import           Data.Pool                                  (Pool)
import           Data.String.Interpolate                    (i)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Database.Persist                           (BackendCompatible)
import           Database.Persist.Postgresql                (ConnectionString,
                                                             PersistValue (..),
                                                             Single (..),
                                                             SqlBackend,
                                                             liftSqlPersistMPool,
                                                             rawExecute, rawSql,
                                                             runMigration,
                                                             withPostgresqlConn,
                                                             withPostgresqlPool)
import           InternalAPI.Persistence.CustomerRepository
import           InternalAPI.Persistence.DailyRepository
import           InternalAPI.Persistence.InvoiceRepository

conn :: Text -> Text -> Text -> Text -> Int -> ConnectionString
conn host databaseName userName password port = [i|host=#{host} dbname=#{databaseName} user=#{userName} password=#{password} port=#{port}|]

connDbless :: Text -> Text -> Text -> Int -> DatabaselessConnString
connDbless host userName password port = [i|host=#{host} user=#{userName} password=#{password} port=#{port}|]

type DatabaselessConnString = ConnectionString

--Todo check why select exists(select from where datname ....) does not work
retrieveDatabase :: MonadIO m => ReaderT SqlBackend m [Single Text]
retrieveDatabase = rawSql "select datname from pg_database where datname='timesheetdb'" []

createDatabase :: MonadIO m => ReaderT SqlBackend m ()
createDatabase = rawExecute "create database timesheetdb" []

executeInPool ::
  (MonadIO m, BackendCompatible SqlBackend backend) =>
  Pool backend ->
  ReaderT backend (NoLoggingT (ResourceT IO)) a ->
  m a
executeInPool = flip liftSqlPersistMPool

provisionalCreateDatabase :: DatabaselessConnString -> IO ()
provisionalCreateDatabase connectionString =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT
      ( do
          existsL <- retrieveDatabase
          when (null existsL) $ do
            res <- createDatabase
            liftIO $ print res
      )
      backend

migrate :: ConnectionString -> IO ()
migrate connectionString = runStderrLoggingT $ withPostgresqlPool connectionString 10 $ liftSqlPersistMPool $ do
  runMigration migrateCustomer
  runMigration migrateDaily
  runMigration migrateInvoice

truncateTable :: MonadIO m => Text -> ReaderT SqlBackend m ()
truncateTable tableName = rawExecute (Text.intercalate " " ["truncate table", tableName, "CASCADE"]) []

truncateAll :: ConnectionString -> IO ()
truncateAll connString =  runStderrLoggingT $ withPostgresqlPool connString 10 $ liftSqlPersistMPool $  do
  truncateTable "customer_record"
  truncateTable "daily_record"
  truncateTable "invoice_record"
