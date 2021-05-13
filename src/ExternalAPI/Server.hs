{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module ExternalAPI.Server where

import qualified Application.CustomerService               as CustomerService
import qualified Application.DailyService                  as DailyService
import           Application.Environment
import qualified Application.InvoiceService                as InvoiceService
import qualified Application.MapUtil                       as MapUtil
import qualified Application.MonthlyService                as MonthlyService
import           Application.ServiceClass
import           Control.Applicative
import           Control.Concurrent.STM                    (atomically,
                                                            newTVarIO,
                                                            readTVarIO,
                                                            stateTVar)
import           Control.Monad.IO.Class                    (liftIO)
import           Control.Monad.Logger                      (runStderrLoggingT)
import           Control.Monad.Reader                      (asks, runReaderT)
import qualified Data.Map                                  as Map
import           Data.Maybe                                (fromMaybe, isJust)
import qualified Data.Text                                 as Text
import           Data.Time                                 (Day)
import           Data.UUID                                 (UUID)
import qualified Data.UUID.V4                              as UUID
import           Database.Persist.Postgresql               (ConnectionString,
                                                            createPostgresqlPool)
import           Debug.Trace
import           Domain.Customer                           (Customer)
import           Domain.Daily                              (allWorkTypes)
import           Domain.Invoice                            (Invoice)
import           Domain.Monthly
import           Domain.MonthlyReport
import           ExternalAPI.ApiType
import           ExternalAPI.FrontEndTypes.DailyJson
import           ExternalAPI.FrontEndTypes.MonthlyListJson (MonthlyListJson,
                                                            from)
import           ExternalAPI.NewTypes.NewCustomer
import           ExternalAPI.NewTypes.NewDaily
import           ExternalAPI.NewTypes.NewInvoice           (NewInvoice (..))
import           ExternalAPI.WorkTypeJson
import qualified InternalAPI.Persistence.Database          as Database
import qualified Network.HTTP.Types                        as HttpTypes
import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger                        (withStdoutLogger)
import           Network.Wai.Middleware.Cors
import           Numeric.Natural                           (Natural)
import           Servant

runWithState :: State -> AppM a -> Handler a
runWithState s x = do
  runReaderT x s

listWorkType :: AppM (XTotalCountHeader [WorkTypeJson])
listWorkType = do
  let workTypes = Text.pack . show <$> allWorkTypes
  liftIO $ print workTypes
  let workTypeJson = uncurry WorkTypeJson <$> zip workTypes workTypes
  liftIO $ print workTypeJson
  return $ addHeader (length workTypes) workTypeJson

listM :: (ReadonlyService a, InEnvironment a) => Maybe Natural -> Maybe Natural -> AppM (XTotalCountHeader [FrontEndType a])
listM start end = do
  m <- asks fetchInMemStore
  (amount, values) <- liftIO $ atomically $ stateTVar m $ list start end
  return $ addHeader amount values

insertM :: (StandardService a, InEnvironment a) => NewType a -> AppM (FrontEndType a)
insertM newA = do
  uuidMap <- asks fetchInMemStore
  uuid <- liftIO UUID.nextRandom
  liftIO $ atomically $ stateTVar uuidMap $ insert uuid newA

deleteM :: (StandardService a, InEnvironment a) => UUID -> AppM (FrontEndType a)
deleteM uuid = do
  uuidMap <- asks fetchInMemStore
  maybeF <- liftIO $ atomically $ stateTVar uuidMap $ delete uuid
  maybe (throwError $ err404 {errBody = "Could not find input id"}) return maybeF

getM :: (StandardService a, InEnvironment a) => UUID -> AppM (FrontEndType a)
getM uuid = do
  uuidMap <- asks fetchInMemStore
  maybeF <- liftIO $ atomically $ stateTVar uuidMap $ get uuid
  maybe (throwError $ err404 {errBody = "Could not find input id"}) return maybeF

listDaily :: Maybe Natural -> Maybe Natural -> AppM (XTotalCountHeader [DailyJson])
listDaily (Just start) (Just end) = do
  (total, dailies) <- DailyService.list start end
  return $ addHeader total $ fromDaily <$> dailies
listDaily _ _ = throwError $ err404 {errBody = "listing needs to provide a start and end"}

deleteDaily :: UUID -> AppM DailyJson
deleteDaily dailyId = do
  maybeRes <- DailyService.delete dailyId
  maybe (throwError $ err404 {errBody = "Could not find input id"}) (return . fromDaily) maybeRes

getDaily :: UUID -> AppM DailyJson
getDaily dailyId = do
  maybeRes <- DailyService.get dailyId
  maybe (throwError $ err404 {errBody = "Could not find input id"}) (return . fromDaily) maybeRes

insertDailyWithGuard :: NewDaily -> AppM DailyJson
insertDailyWithGuard newDaily@(NewDaily d _ cId cVat) = do
--  TODO Do this better
--  maybeDaily <- DailyService.get cId cVat d
--  if isJust maybeDaily
--    then throwError $ err409 {errBody = "Day already filled out, edit existing instead of writing new"}
--    else do
      daily <- DailyService.insert newDaily
      return $ fromDaily daily

listCustomer :: Maybe Natural -> Maybe Natural -> AppM (XTotalCountHeader [Customer])
listCustomer (Just start) (Just end) = do
  (total, customers) <- CustomerService.list start end
  return $ addHeader total customers
listCustomer _ _ = throwError $ err404 {errBody = "listing needs to provide a start and end"}

deleteCustomer :: UUID -> AppM Customer
deleteCustomer businessId = do
  maybeRes <- CustomerService.delete businessId
  maybe (throwError $ err404 {errBody = "Could not find input id"}) return maybeRes

getCustomer :: UUID -> AppM Customer
getCustomer businessId = do
  maybeRes <- CustomerService.get businessId
  maybe (throwError $ err404 {errBody = "Could not find input id"}) return maybeRes

insertCustomer :: NewCustomer -> AppM Customer
insertCustomer = CustomerService.insert

serveDaily :: ServerT DailyApi AppM
serveDaily = listDaily :<|> insertDailyWithGuard :<|> deleteDaily :<|> getDaily

serveCustomerApi :: ServerT CustomerApi AppM
serveCustomerApi = listCustomer :<|> insertCustomer :<|> getCustomer

serveWorkTypes :: ServerT WorkTypeApi AppM
serveWorkTypes = listWorkType

listMonthlies :: Maybe Natural -> Maybe Natural -> AppM (XTotalCountHeader [MonthlyListJson])
listMonthlies (Just start) (Just end) = do
  allMonthsWithOneDay <- MonthlyService.selectMonthsWithUninvoicedWork
  let selection = take (fromEnum $ end - start) . drop (fromEnum start) $ allMonthsWithOneDay
  return $ addHeader (length allMonthsWithOneDay) $ from <$> selection

listInvoices :: Maybe Natural -> Maybe Natural -> AppM (XTotalCountHeader [Invoice])
listInvoices (Just start) (Just end) = do
  (t, invoices) <- InvoiceService.list start end
  liftIO $ print t
  liftIO $ print invoices
  return $ addHeader t invoices

createInvoice :: NewInvoice -> AppM Invoice
createInvoice = InvoiceService.insert

getInvoice :: SpecificMonth -> AppM Invoice
getInvoice sMonth = do
  maybeInvoice <- InvoiceService.get sMonth
  maybe (throwError $ err404 {errBody = "Could not find invoice"}) return maybeInvoice

--getMonthly :: SpecificMonth -> AppM MonthlyReport
--getMonthly = MonthlyService.getReport

serveMonthlyApi :: ServerT MonthlyApi AppM
serveMonthlyApi = listMonthlies

serveInvoiceApi :: ServerT InvoiceApi AppM
serveInvoiceApi = listInvoices :<|> createInvoice :<|> getInvoice

server :: String -> ServerT WebApi AppM
server webAppLocation =
  serveDaily :<|> serveWorkTypes :<|> serveMonthlyApi :<|> serveCustomerApi :<|> serveInvoiceApi

corsConfig :: Middleware
corsConfig = cors (const $ Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsExposedHeaders = Just ["X-Total-Count", "Access-Control-Allow-Origin"],
          corsMethods = [HttpTypes.methodGet, HttpTypes.methodPost, HttpTypes.methodHead, HttpTypes.methodOptions, HttpTypes.methodDelete, HttpTypes.methodPut],
          corsRequestHeaders = [HttpTypes.hContentType]
        }

admin :: State -> Application
admin initialState req respond = do
  serve
    api
    (hoistServer api (runWithState initialState) (server "web-dist"))
    req
    respond

--TODO turn this into env vars read or so later on
defaultDatabaseConnectionString :: ConnectionString
defaultDatabaseConnectionString = Database.conn "localhost" "timesheetdb" "postgres" "mysecretpassword" 5432

start :: Int -> IO ()
start port = withStdoutLogger $ \aplogger -> do
  initialMonthlyMap <- newTVarIO Map.empty
  let connectionString = defaultDatabaseConnectionString
  --  TODO check how to shut down gracefully when the server shuts down. Some kind of shutdown hook will likely exist. Maybe a withPool around this whole snippet is the best?
  poel <- runStderrLoggingT $ createPostgresqlPool connectionString 10
  let initialState = State initialMonthlyMap poel
  let settings = setPort port $ setLogger aplogger defaultSettings
  runSettings settings $ corsConfig $ admin initialState
