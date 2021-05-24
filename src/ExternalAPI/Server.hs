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

import qualified Application.CompanyService                as CompanyService
import qualified Application.CustomerService               as CustomerService
import qualified Application.DailyService                  as DailyService
import           Application.Environment
import qualified Application.FixedPriceInvoiceService      as FixedPriceInvoiceService
import qualified Application.InvoiceService                as InvoiceService
import qualified Application.MapUtil                       as MapUtil
import qualified Application.MonthlyService                as MonthlyService
import qualified Application.QuoteService                  as QuoteService
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
import           Domain.Company                            (Company)
import           Domain.Customer                           (Customer)
import           Domain.Daily                              (allWorkTypes)
import           Domain.FixedPriceInvoice                  (FixedPriceInvoice)
import           Domain.Invoice                            (Invoice)
import           Domain.Monthly
import           Domain.MonthlyId
import           Domain.MonthlyReport
import           Domain.Quote                              (Quote)
import           ExternalAPI.ApiType
import           ExternalAPI.FrontEndTypes.DailyJson
import           ExternalAPI.FrontEndTypes.MonthlyListJson (MonthlyListJson,
                                                            from)
import           ExternalAPI.NewTypes.NewCompany
import           ExternalAPI.NewTypes.NewCustomer
import           ExternalAPI.NewTypes.NewDaily
import           ExternalAPI.NewTypes.NewFixedPriceInvoice (NewFixedPriceInvoice (..))
import           ExternalAPI.NewTypes.NewInvoice           (NewInvoice (..))
import           ExternalAPI.NewTypes.NewQuote             (NewQuote (..))
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
listCustomer _ _ = do
  allCustomers <- CustomerService.listAll
  return $ addHeader (length allCustomers) allCustomers

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

listCompany :: Maybe Natural -> Maybe Natural -> AppM (XTotalCountHeader [Company])
listCompany (Just start) (Just end) = do
  (total, companies) <- CompanyService.list start end
  return $ addHeader total companies
listCompany _ _ = do
  allCompanies <- CompanyService.listAll
  return $ addHeader (length allCompanies) allCompanies

getCompany :: UUID -> AppM Company
getCompany businessId = do
  maybeRes <- CompanyService.get businessId
  maybe (throwError $ err404 {errBody = "Could not find input id"}) return maybeRes

insertCompany :: NewCompany -> AppM Company
insertCompany = CompanyService.insert

serveCompanyApi :: ServerT CompanyApi AppM
serveCompanyApi = listCompany :<|> insertCompany :<|> getCompany

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
  return $ addHeader t invoices

listFixedPriceInvoices :: Maybe Natural -> Maybe Natural -> AppM (XTotalCountHeader [FixedPriceInvoice])
listFixedPriceInvoices (Just start) (Just end) = do
  (t, fixedPriceInvoices) <- FixedPriceInvoiceService.list start end
  return $ addHeader t fixedPriceInvoices

listQuotes :: Maybe QuoteFilter -> Maybe Natural -> Maybe Natural -> AppM (XTotalCountHeader [Quote])
listQuotes Nothing (Just start) (Just end) = do
  (t, quotes) <- QuoteService.list start end
  return $ addHeader t quotes
listQuotes (Just Uninvoiced) _ _ = do
  quotes <- QuoteService.listNonInvoiced
  return $ addHeader (length quotes) quotes

createInvoice :: NewInvoice -> AppM Invoice
createInvoice (NewInvoice monthlyId)= InvoiceService.insert monthlyId

createFixedPriceInvoice :: NewFixedPriceInvoice -> AppM FixedPriceInvoice
createFixedPriceInvoice = FixedPriceInvoiceService.insert

createQuote :: NewQuote -> AppM Quote
createQuote = QuoteService.insert

getInvoice :: UUID -> AppM Invoice
getInvoice invoiceId = do
  maybeInvoice <- InvoiceService.get invoiceId
  maybe (throwError $ err404 {errBody = "Could not find invoice"}) return maybeInvoice

getFixedPriceInvoice :: UUID -> AppM FixedPriceInvoice
getFixedPriceInvoice fixedPriceInvoiceId = do
  maybeFixedPriceInvoice <- FixedPriceInvoiceService.get fixedPriceInvoiceId
  maybe (throwError $ err404 {errBody = "Could not find ficed price invoice"}) return maybeFixedPriceInvoice

getQuote :: UUID -> AppM Quote
getQuote quoteId = do
  maybeQuote <- QuoteService.get quoteId
  maybe (throwError $ err404 {errBody = "Could not find quote"}) return maybeQuote

--getMonthly :: SpecificMonth -> AppM MonthlyReport
--getMonthly = MonthlyService.getReport

serveMonthlyApi :: ServerT MonthlyApi AppM
serveMonthlyApi = listMonthlies

serveInvoiceApi :: ServerT InvoiceApi AppM
serveInvoiceApi = listInvoices :<|> createInvoice :<|> getInvoice

serveQuoteApi :: ServerT QuoteApi AppM
serveQuoteApi = listQuotes :<|> createQuote :<|> getQuote

serveFixedPriceInvoiceApi :: ServerT FixedPriceInvoiceApi AppM
serveFixedPriceInvoiceApi = listFixedPriceInvoices :<|> createFixedPriceInvoice :<|> getFixedPriceInvoice

server :: String -> ServerT WebApi AppM
server webAppLocation =
  serveDaily :<|> serveWorkTypes :<|> serveMonthlyApi :<|> serveCustomerApi :<|> serveInvoiceApi :<|> serveCompanyApi :<|> serveQuoteApi :<|> serveFixedPriceInvoiceApi

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
defaultDatabaseConnectionString :: Int -> ConnectionString
defaultDatabaseConnectionString = Database.conn "localhost" "timesheetdb" "postgres" "mysecretpassword"

start :: Int -> Int -> IO ()
start port dbPort = withStdoutLogger $ \aplogger -> do
  initialMonthlyMap <- newTVarIO Map.empty
  let connectionString = defaultDatabaseConnectionString dbPort
  --  TODO check how to shut down gracefully when the server shuts down. Some kind of shutdown hook will likely exist. Maybe a withPool around this whole snippet is the best?
  poel <- runStderrLoggingT $ createPostgresqlPool connectionString 10
  let initialState = State initialMonthlyMap poel
  let settings = setPort port $ setLogger aplogger defaultSettings
  runSettings settings $ corsConfig $ admin initialState
