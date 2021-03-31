{-# LANGUAGE OverloadedStrings #-}

module Helper.DatabaseHelper where

import           Application.Environment
import           Control.Concurrent.STM.TVar         (newTVarIO)
import           Control.Exception                   (bracket)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Logger                (NoLoggingT,
                                                      runStderrLoggingT)
import           Control.Monad.Reader                (ReaderT, runReaderT)
import           Control.Monad.Trans.Resource        (ResourceT)
import qualified Data.Map                            as Map
import           Database.Persist.Postgresql         (ConnectionString,
                                                      createPostgresqlPool,
                                                      withPostgresqlPool)
import           Database.Persist.Sql                (SqlBackend,
                                                      liftSqlPersistMPool)
import qualified InternalAPI.Persistence.Database    as DB
import           Servant.Server.Internal.Handler     (runHandler)
import           Servant.Server.Internal.ServerError (ServerError)


hasSize:: Foldable t => Int -> t a -> Bool
hasSize i foldable = length foldable == i

runWithoutPool :: ConnectionString -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runWithoutPool connectionString dbAction = liftIO $ runStderrLoggingT $ withPostgresqlPool connectionString 10 $ liftSqlPersistMPool dbAction

--TODO this does not really belong in database helper anymore. Maybe it is more test helper now. Or maybe two modules
runAppM :: State -> AppM a -> IO (Either ServerError a)
runAppM  s appM = runHandler $ runReaderT appM s


setupDatabase :: IO ConnectionString
setupDatabase = do
  let hostName = "localhost"
  let userName = "postgres"
  let password = "mysecretpassword"
  let dbName = "timesheetdb"
  let dbLessConnString = DB.connDbless hostName userName password 5432
  let connString = DB.conn hostName dbName userName password 5432
  DB.provisionalCreateDatabase dbLessConnString
  DB.migrate connString
  return connString

teardownDatabase :: ConnectionString -> IO ()
teardownDatabase connString = do
  DB.truncateAll connString

withDatabase :: (ConnectionString -> IO ()) -> IO ()
withDatabase testAction = do
  bracket setupDatabase teardownDatabase testAction

createInitialState :: ConnectionString -> IO State
createInitialState connectionString = do
    initialMonthlyMap <- newTVarIO Map.empty
    --  TODO check how to shut down gracefully when the server shuts down. Some kind of shutdown hook will likely exist. Maybe a withPool around this whole snippet is the best?
    poel <- runStderrLoggingT $ createPostgresqlPool connectionString 10
    return $ State initialMonthlyMap poel
