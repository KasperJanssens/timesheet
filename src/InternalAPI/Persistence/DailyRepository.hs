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
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Reader                       (ReaderT)
import qualified Data.List                                  as List
import           Data.Maybe                                 (catMaybes,
                                                             fromJust)
import           Data.Text                                  (Text)
import           Data.Time                                  (getCurrentTime)
import           Data.Time.Calendar
import           Data.Time.Clock                            (UTCTime)
import           Data.UUID                                  (UUID)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Company                             (Company)
import qualified Domain.Company                             as Company
import           Domain.Customer                            (Customer)
import qualified Domain.Customer                            as Customer
import           Domain.Daily
import           InternalAPI.Persistence.BusinessId         (BusinessId (..))
import           InternalAPI.Persistence.CompanyRepository  hiding (to)
import           InternalAPI.Persistence.CustomerRepository hiding (to)
import           Safe                                       (headMay)
import           Servant.Server.Internal.ServerError        (err404, errBody)

share
  [mkPersist sqlSettings, mkMigrate "migrateDaily"]
  [persistLowerCase|
WorkPackRecord
    amount Double
    workType String
    description Text
    dailyLink DailyRecordId
    deriving Show
DailyRecord
    day Day
    creationDate UTCTime
    monthNumber Int
    year Int
    customerLink CustomerRecordId
    companyLink CompanyRecordId
    UniqueDay day companyLink customerLink
    deriving Show
|]

toWorkPack :: WorkPackRecord -> WorkPack
toWorkPack (WorkPackRecord a t d _) = WorkPack a (read t) d

fromWorkPack :: Key DailyRecord -> WorkPack -> WorkPackRecord
fromWorkPack dailyRecordId (WorkPack a t d) = WorkPackRecord a (show t) d dailyRecordId

toDaily :: UUID -> Text -> [WorkPack] -> Day -> Daily
toDaily customerId companyVat workPacks d = Daily d workPacks customerId companyVat

to :: CustomerRecord -> CompanyRecord -> [WorkPackRecord] -> DailyRecord -> Daily
to customerRecord companyRecord workPackRecords dailyRecord =
  let workPacks = toWorkPack <$> workPackRecords
   in toDaily (uuid $ customerRecordBusinessId customerRecord) (companyRecordVatNumber companyRecord) workPacks (dailyRecordDay dailyRecord)

workPacksForMonth :: (MonadIO m) => UUID -> Text -> Integer -> Int -> ReaderT SqlBackend m [Daily]
workPacksForMonth customerId companyVat year month = do
  maybeCustomer <- getBy (UniqueCustomerBusinessId (BusinessId customerId))
  maybeCompany <- getBy (UniqueCompanyVAT companyVat)
  let customerRecord = fromJust maybeCustomer
  let companyRecord = fromJust maybeCompany
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

findByDay :: (MonadIO m) => Day -> UUID -> Text -> ReaderT SqlBackend m (Maybe Daily)
findByDay day customerId companyVat = do
  --  Get rid of the fromJust
  maybeCustomer <- getBy (UniqueCustomerBusinessId (BusinessId customerId))
  maybeCompany <- getBy (UniqueCompanyVAT companyVat)
  let customerRecord = fromJust maybeCustomer
  let companyRecord = fromJust maybeCompany
  let uniqueConstraint = UniqueDay day (entityKey companyRecord) (entityKey customerRecord)
  --  maybeUniqueConstraint <- createDayUniqueConstraint day customerId companyVat
  --  TODO can we do this better? Looks a bit annoying, but there is no liftMaybe or so
  maybeDaily <- getBy uniqueConstraint
  maybe
    (return Nothing)
    ( \dailyEntity -> do
        daily <- addWorkPacks dailyEntity (entityVal companyRecord) (entityVal customerRecord)
        return $ Just daily
    )
    maybeDaily

insertDaily :: (MonadIO m) => Daily -> ReaderT SqlBackend m DailyRecordId
insertDaily (Daily day workPacks customerId companyVat) = do
  now <- liftIO getCurrentTime
  let (year, month, _) = toGregorian day
  maybeCustomer <- getBy (UniqueCustomerBusinessId (BusinessId customerId))
  maybeCompany <- getBy (UniqueCompanyVAT companyVat)
  let maybeDailyRecord = liftA2 (DailyRecord day now month (fromIntegral year)) (entityKey <$> maybeCustomer) (entityKey <$> maybeCompany)
  --  TODO this needs to be better
  let dailyRecord = fromJust maybeDailyRecord
  dailyRecordId <- insert dailyRecord
  let workPackRecords = fromWorkPack dailyRecordId <$> workPacks
  insertMany_ workPackRecords
  return dailyRecordId

yearAndMonth :: Maybe DailyRecord -> Maybe (Int, Int)
yearAndMonth maybeRecord = do
  r <- maybeRecord
  return (dailyRecordMonthNumber r, dailyRecordYear r)

distinctMonthsAndYears :: [[DailyRecord]] -> [Maybe (Int, Int)]
distinctMonthsAndYears groupedRecords = (\rs -> let hMaybe = headMay rs in yearAndMonth hMaybe) <$> groupedRecords

allMonthsWithWorkedDays :: (MonadIO m) => ReaderT SqlBackend m [(Int, Int)]
allMonthsWithWorkedDays = do
  entities <- selectList [] [Desc DailyRecordYear, Desc DailyRecordMonthNumber]
  let records = entityVal <$> entities
  let groupedRecords = List.groupBy (\left right -> dailyRecordMonthNumber left == dailyRecordMonthNumber right && dailyRecordYear left == dailyRecordYear right) records
  let distinctMonths = distinctMonthsAndYears groupedRecords
  return $ catMaybes distinctMonths

allDailies :: [Filter DailyRecord]
allDailies = []

countDailies :: MonadIO m => ReaderT SqlBackend m Int
countDailies = count allDailies

fetchCompany :: (MonadIO m) => Entity DailyRecord -> ReaderT SqlBackend m CompanyRecord
fetchCompany dailyRecordEntity =
  let companyLink = dailyRecordCompanyLink (entityVal dailyRecordEntity)
   in do
        maybeCompany <- get companyLink
        --      TODO Get rid of from just
        return $ fromJust maybeCompany

fetchCustomer :: (MonadIO m) => Entity DailyRecord -> ReaderT SqlBackend m CustomerRecord
fetchCustomer dailyRecordEntity =
  let customerLink = dailyRecordCustomerLink (entityVal dailyRecordEntity)
   in do
        maybeCompany <- get customerLink
        --      TODO Get rid of from just
        return $ fromJust maybeCompany

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

getDailies :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Daily]
getDailies start stop = do
  records <- selectList [] [Asc DailyRecordCreationDate, OffsetBy start, LimitTo (stop - start)]
  companies <- mapM fetchCompany records
  customers <- mapM fetchCustomer records
  let daList = zip3 records companies customers
  mapM (uncurry3 addWorkPacks) daList

deleteDaily :: (MonadIO m) => Day -> UUID -> Text -> ReaderT SqlBackend m ()
deleteDaily day customerId companyVat = do
  maybeUniqueConstraint <- createDayUniqueConstraint day customerId companyVat
  --  TODO can we do this better? Looks a bit annoying, but there is no liftMaybe or so. ALso lots of duplication with the get
  maybeDailyEntity <-
    maybe
      (return Nothing)
      getBy
      maybeUniqueConstraint
  maybe
    (pure ())
    ( \dailyEntity -> do
        let dailyRecordId = entityKey dailyEntity
        deleteWhere [WorkPackRecordDailyLink ==. dailyRecordId]
        delete dailyRecordId
    )
    maybeDailyEntity
