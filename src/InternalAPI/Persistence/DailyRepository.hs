{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module InternalAPI.Persistence.DailyRepository where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (ReaderT)
import qualified Data.List                   as List
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import           Data.Time                   (getCurrentTime)
import           Data.Time.Calendar
import           Data.Time.Clock             (UTCTime)
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Domain.Daily
import           Safe                        (headMay)

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
    UniqueDay day
    deriving Show
|]

toWorkPack :: WorkPackRecord -> WorkPack
toWorkPack (WorkPackRecord a t d _) = WorkPack a (read t) d

fromWorkPack :: Key DailyRecord -> WorkPack -> WorkPackRecord
fromWorkPack dailyRecordId (WorkPack a t d) = WorkPackRecord a (show t) d dailyRecordId



toDaily :: [WorkPack] -> DailyRecord -> Daily
toDaily workPacks (DailyRecord d _ _ _) = Daily d workPacks

to :: [WorkPackRecord] -> DailyRecord -> Daily
to workPackRecords dailyrecord =
  let workPacks = toWorkPack <$> workPackRecords
   in toDaily workPacks dailyrecord

workPacksForMonth :: (MonadIO m) =>  Integer -> Int -> ReaderT SqlBackend m [Daily]
workPacksForMonth year month = do
  records <- selectList  [DailyRecordMonthNumber ==. month, DailyRecordYear  ==. fromIntegral year] []
  mapM addWorkPacks records

addWorkPacks :: (MonadIO m) => Entity DailyRecord -> ReaderT SqlBackend m Daily
addWorkPacks dailyEntity = do
  let dailyDBId = entityKey dailyEntity
  workPackEntities <- selectList [WorkPackRecordDailyLink ==. dailyDBId] []
  let workPacks = entityVal <$> workPackEntities
  return $ to workPacks $ entityVal dailyEntity

findByDay :: (MonadIO m) => Day -> ReaderT SqlBackend m (Maybe Daily)
findByDay day = do
  maybeDaily <- getBy (UniqueDay day)
  maybe
    (return Nothing)
    ( \dailyEntity -> do
        daily <- addWorkPacks dailyEntity
        return $ Just daily
    )
    maybeDaily

insertDaily :: (MonadIO m) => Daily -> ReaderT SqlBackend m DailyRecordId
insertDaily (Daily day workPacks) = do
  now <- liftIO getCurrentTime
  let (year, month, _) = toGregorian day
  dailyRecordId <- insert (DailyRecord day now month (fromIntegral year))
  let workPackRecords = fromWorkPack dailyRecordId <$> workPacks
  insertMany_ workPackRecords
  return dailyRecordId

yearAndMonth :: Maybe DailyRecord -> Maybe (Int, Int)
yearAndMonth maybeRecord = do
  r <- maybeRecord
  return (dailyRecordMonthNumber r, dailyRecordYear r)

distinctMonthsAndYears :: [[DailyRecord]] -> [Maybe (Int, Int)]
distinctMonthsAndYears groupedRecords =  (\rs -> let hMaybe = headMay rs in yearAndMonth hMaybe) <$> groupedRecords

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

getDailies :: MonadIO m => Int -> Int -> ReaderT SqlBackend m [Daily]
getDailies start stop = do
  records <- selectList [] [Asc DailyRecordCreationDate, OffsetBy start, LimitTo (stop - start)]
  mapM addWorkPacks records

deleteDaily :: (MonadIO m) => Day -> ReaderT SqlBackend m ()
deleteDaily day = do
  maybeDailyEntity <- getBy (UniqueDay day)
  maybe
    (pure ())
    ( \dailyEntity -> do
        let dailyRecordId = entityKey dailyEntity
        deleteWhere [WorkPackRecordDailyLink ==. dailyRecordId]
        delete dailyRecordId
    )
    maybeDailyEntity
