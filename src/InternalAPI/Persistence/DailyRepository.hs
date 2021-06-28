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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module InternalAPI.Persistence.DailyRepository where

import           Control.Applicative
import           Control.Exception.Base                     (throw)
import           Control.Monad                              (foldM, forM_)
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Reader                       (ReaderT)
import qualified Data.List                                  as List
import           Data.Maybe                                 (catMaybes)
import           Data.Text                                  (Text)
import           Data.Time                                  (getCurrentTime)
import           Data.Time.Calendar
import           Data.Time.Clock                            (UTCTime)
import           Data.UUID                                  (UUID)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Daily
import qualified Domain.Daily                               as Daily
import           InternalAPI.Persistence.BusinessId         (BusinessId (..))
import           InternalAPI.Persistence.CompanyRepository  hiding (to)
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import           InternalAPI.Persistence.InvoiceRepository  (InvoiceRecordId, Unique (UniqueInvoiceBusinessId))
import           InternalAPI.Persistence.RepositoryError
import           Safe                                       (headMay)

share
  [mkPersist sqlSettings, mkMigrate "migrateDaily"]
  [persistLowerCase|
WorkPackRecord
    businessId BusinessId
    amount Double
    workType String
    description Text
    dailyLink DailyRecordId
    invoiceLink InvoiceRecordId Maybe
    UniqueWorkPackBusinessId businessId
    deriving Show
DailyRecord
    businessId BusinessId
    day Day
    creationDate UTCTime
    monthNumber Int
    year Int
    alreadyInvoiced Bool
    customerLink CustomerRecordId
    companyLink CompanyRecordId
    UniqueDay day companyLink customerLink
    UniqueDailyBusinessId businessId
    deriving Show
|]

conditionallyDelete :: (MonadIO m) => Daily -> ReaderT SqlBackend m ()
conditionallyDelete (Daily dailyId _ _ _ _ False) = deleteDaily dailyId
conditionallyDelete (Daily _ _ _ _ _ True)        = return ()

toWorkPack :: WorkPackRecord -> WorkPack
toWorkPack (WorkPackRecord (BusinessId bId) a t d _ _) = WorkPack bId a (read t) d

fromWorkPack :: Key DailyRecord -> WorkPack -> WorkPackRecord
fromWorkPack dailyRecordId (WorkPack bId a t d) = WorkPackRecord (BusinessId bId) a (show t) d dailyRecordId Nothing

toDaily :: UUID -> UUID -> Text -> [WorkPack] -> Day -> Bool -> Daily
toDaily dailyId customerId companyVat workPacks d alreadyInvoiced = Daily dailyId d workPacks customerId companyVat alreadyInvoiced

to :: CustomerRecord -> CompanyRecord -> [WorkPackRecord] -> DailyRecord -> Daily
to customerRecord companyRecord workPackRecords dailyRecord =
  let workPacks = toWorkPack <$> workPackRecords
   in toDaily (uuid $ dailyRecordBusinessId dailyRecord) (uuid $ customerRecordBusinessId customerRecord) (companyRecordVatNumber companyRecord) workPacks (dailyRecordDay dailyRecord) (dailyRecordAlreadyInvoiced dailyRecord)

workPacksForMonth :: (MonadIO m) => UUID -> UUID -> Int -> Int -> ReaderT SqlBackend m [Daily]
workPacksForMonth customerId companyId year month = do
  maybeCustomer <- getBy (UniqueCustomerBusinessId (BusinessId customerId))
  maybeCompany <- getBy (UniqueCompanyBusinessId (BusinessId companyId))
  customerRecord <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomer
  companyRecord <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompany
  records <-
    selectList
      [ DailyRecordMonthNumber ==. month,
        DailyRecordYear ==. fromIntegral year,
        DailyRecordCustomerLink ==. entityKey customerRecord,
        DailyRecordCompanyLink ==. entityKey companyRecord
      ]
      []
  mapM (\d -> addWorkPacks d (entityVal companyRecord) (entityVal customerRecord)) records

addWorkPacks :: (MonadIO m) => Entity DailyRecord -> CompanyRecord -> CustomerRecord -> ReaderT SqlBackend m Daily
addWorkPacks dailyEntity companyRecord customerRecord = do
  let dailyDBId = entityKey dailyEntity
  workPackEntities <- selectList [WorkPackRecordDailyLink ==. dailyDBId] []
  let workPacks = entityVal <$> workPackEntities
  return $ to customerRecord companyRecord workPacks (entityVal dailyEntity)

createDayUniqueConstraint :: (MonadIO m) => Day -> UUID -> Text -> ReaderT SqlBackend m (Maybe (Unique DailyRecord))
createDayUniqueConstraint day customerId companyVat = do
  maybeCustomer <- getBy (UniqueCustomerBusinessId (BusinessId customerId))
  maybeCompany <- getBy (UniqueCompanyVAT companyVat)
  return $ liftA2 (UniqueDay day) (entityKey <$> maybeCompany) (entityKey <$> maybeCustomer)

findByDay :: (MonadIO m) => UUID -> ReaderT SqlBackend m (Maybe Daily)
findByDay dailyId = do
  --  Get rid of the fromJust
  --  maybeCustomer <- getBy (UniqueCustomerBusinessId (BusinessId customerId))
  --  maybeCompany <- getBy (UniqueCompanyVAT companyVat)
  --  let customerRecord = fromJust maybeCustomer
  --  let companyRecord = fromJust maybeCompany
  --  let uniqueConstraint = UniqueDay day (entityKey companyRecord) (entityKey customerRecord)
  --  maybeUniqueConstraint <- createDayUniqueConstraint day customerId companyVat
  --  TODO can we do this better? Looks a bit annoying, but there is no liftMaybe or so
  maybeDaily <- getBy (UniqueDailyBusinessId $ BusinessId dailyId)
  maybe
    (return Nothing)
    ( \dailyEntity -> do
        maybeCustomer <- get (dailyRecordCustomerLink $ entityVal dailyEntity)
        maybeCompany <- get (dailyRecordCompanyLink $ entityVal dailyEntity)
        customerRecord <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomer
        companyRecord <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompany
        daily <- addWorkPacks dailyEntity companyRecord customerRecord
        return $ Just daily
    )
    maybeDaily

insertDaily :: (MonadIO m) => Daily -> ReaderT SqlBackend m DailyRecordId
insertDaily (Daily uuid day workPacks customerId companyVat alreadyInvoiced) = do
  now <- liftIO getCurrentTime
  let (year, month, _) = toGregorian day
  maybeCustomer <- getBy (UniqueCustomerBusinessId (BusinessId customerId))
  maybeCompany <- getBy (UniqueCompanyVAT companyVat)
  let maybeDailyRecord = liftA2 (DailyRecord (BusinessId uuid) day now month (fromIntegral year) alreadyInvoiced) (entityKey <$> maybeCustomer) (entityKey <$> maybeCompany)
  dailyRecord <-  liftIO $ maybe (throw $ RepositoryError "Daily record noet found") return  maybeDailyRecord
  dailyRecordId <- insert dailyRecord
  let workPackRecords = fromWorkPack dailyRecordId <$> workPacks
  insertMany_ workPackRecords
  return dailyRecordId

yearAndMonth :: Maybe DailyRecord -> Maybe (Int, Int, CustomerRecordId, CompanyRecordId)
yearAndMonth maybeRecord = do
  r <- maybeRecord
  return (dailyRecordMonthNumber r, dailyRecordYear r, dailyRecordCustomerLink r, dailyRecordCompanyLink r)

distinctMonthsAndYears :: [[DailyRecord]] -> [Maybe (Int, Int, CustomerRecordId, CompanyRecordId)]
distinctMonthsAndYears groupedRecords = (\rs -> let hMaybe = headMay rs in yearAndMonth hMaybe) <$> groupedRecords

fetchCompanyAndCustomer :: (MonadIO m) => CustomerRecordId -> CompanyRecordId -> ReaderT SqlBackend m (CustomerRecord, CompanyRecord)
fetchCompanyAndCustomer customerRecordId companyRecordId = do
  maybeCustomerRecord <- get customerRecordId
  maybeCompanyRecord <- get companyRecordId
  customerRecord <- liftIO $ maybe (throw $ RepositoryError "Customer not found") return maybeCustomerRecord
  companyRecord <- liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompanyRecord
  return (customerRecord, companyRecord)

fetchCompany :: (MonadIO m) => Entity DailyRecord -> ReaderT SqlBackend m CompanyRecord
fetchCompany dailyRecordEntity = do
  let companyLink = dailyRecordCompanyLink (entityVal dailyRecordEntity)
  maybeCompany <- get companyLink
  liftIO $ maybe (throw $ RepositoryError "Company not found") return maybeCompany

selectMonthsWithUninvoicedWorkPacks :: (MonadIO m) => ReaderT SqlBackend m [Entity DailyRecord]
selectMonthsWithUninvoicedWorkPacks = do
  rawSql "select ?? from daily_record , work_pack_record  where work_pack_record.daily_link=daily_record.id and work_pack_record.invoice_link is null order by daily_record.year, daily_record.month_number desc" []

allMonthsWithWorkedDays :: (MonadIO m) => ReaderT SqlBackend m [(Int, Int, CustomerRecord, CompanyRecord)]
allMonthsWithWorkedDays = do
  entities <- selectMonthsWithUninvoicedWorkPacks
  liftIO $ print "This is the result of the raw sql select"
  liftIO $ print entities
  liftIO $ print "------------------------------------------"
  --  entities <- selectList [] [Desc DailyRecordYear, Desc DailyRecordMonthNumber]
  let records = entityVal <$> entities
  let groupedRecords =
        List.groupBy
          ( \left right ->
              dailyRecordMonthNumber left == dailyRecordMonthNumber right
                && dailyRecordYear left == dailyRecordYear right
                && dailyRecordCustomerLink left == dailyRecordCustomerLink right
                && dailyRecordCompanyLink left == dailyRecordCompanyLink right
          )
          records
  liftIO $ print "These are the grouped records"
  liftIO $ print groupedRecords
  liftIO $ print "----------------------------------"
  let distinctMonths = distinctMonthsAndYears groupedRecords
  let res = catMaybes distinctMonths
  foldM
    ( \acc elem@(m, y, customerRecordId, companyRecordId) -> do
        (customer, company) <- fetchCompanyAndCustomer customerRecordId companyRecordId
        return $ acc ++ [(m, y, customer, company)]
    )
    []
    res

allDailies :: [Filter DailyRecord]
allDailies = []

countDailies :: MonadIO m => ReaderT SqlBackend m Int
countDailies = count allDailies



fetchCustomer :: (MonadIO m) => Entity DailyRecord -> ReaderT SqlBackend m CustomerRecord
fetchCustomer dailyRecordEntity =
  let customerLink = dailyRecordCustomerLink (entityVal dailyRecordEntity)
   in do
        maybeCustomer <- get customerLink
        liftIO $ maybe (throw $ RepositoryError "Customer not found ") return maybeCustomer

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

getDailies :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Daily]
getDailies start stop = do
  records <- selectList [] [Asc DailyRecordCreationDate, OffsetBy start, LimitTo (stop - start)]
  companies <- mapM fetchCompany records
  customers <- mapM fetchCustomer records
  let daList = zip3 records companies customers
  mapM (uncurry3 addWorkPacks) daList

deleteDaily :: (MonadIO m) => UUID -> ReaderT SqlBackend m ()
deleteDaily uuid = do
  deleteBy (UniqueDailyBusinessId (BusinessId uuid))

markAsInvoiced ::(MonadIO m) => UUID -> ReaderT SqlBackend m ()
markAsInvoiced businessId = do
   maybeDaily <- getBy (UniqueDailyBusinessId (BusinessId businessId))
   dailyRecord <- liftIO $ maybe (throw $ RepositoryError "Daily not found") return maybeDaily
   let dailyRecordId = entityKey dailyRecord
   update dailyRecordId [DailyRecordAlreadyInvoiced =. True]

markAllAsInvoiced :: (MonadIO m) => [Daily] -> ReaderT SqlBackend m ()
markAllAsInvoiced dailies = do
  let dailyIds = Daily.id <$> dailies
  forM_ dailyIds markAsInvoiced

linkInvoiceToWorkpacks :: (MonadIO m) => [WorkPack] -> UUID -> ReaderT SqlBackend m ()
linkInvoiceToWorkpacks wps invoiceBusinessId = do
  maybeInvoice <- getBy (UniqueInvoiceBusinessId (BusinessId invoiceBusinessId))
  invoiceRecord <- liftIO $ maybe (throw $ RepositoryError "Invoice not found") return maybeInvoice
  let invoiceRecordId = entityKey invoiceRecord
  let workPackBIds = wpid <$> wps
  maybeWorkPackRecordEntities <- mapM (getBy . UniqueWorkPackBusinessId . BusinessId) workPackBIds
  let workPackRecordIds = entityKey <$> catMaybes maybeWorkPackRecordEntities
  forM_ workPackRecordIds (\wpRecordId -> update wpRecordId [WorkPackRecordInvoiceLink =. Just invoiceRecordId])
