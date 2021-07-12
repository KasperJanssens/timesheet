module Application.DailyService where

import           Application.Environment
import           Control.Monad                           (void)
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Logger                    (runStderrLoggingT)
import           Control.Monad.Trans.Reader              (asks)
import           Data.Text                               (Text)
import           Data.Time                               (Day)
import           Data.UUID                               (UUID)
import qualified Data.UUID.V4                            as UUID
import           Database.Persist.Postgresql             (ConnectionString,
                                                          withPostgresqlPool)
import           Domain.Company                          (Company (..))
import           Domain.Customer                         (Customer (..))
import           Domain.Daily                            (Daily (..),
                                                          WorkPack (..))
import           Domain.Monthly                          (SpecificMonth (..))
import           ExternalAPI.NewTypes.NewDaily
import           ExternalAPI.NewTypes.NewDaily           (NewDaily (..))
import           InternalAPI.Persistence.BusinessId
import qualified InternalAPI.Persistence.DailyRepository as DailyRepository
import qualified InternalAPI.Persistence.Database        as DB
import           Numeric.Natural                         (Natural)

list :: Natural -> Natural -> AppM (Int, [Daily])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      dailies <- DailyRepository.getDailies (fromEnum from) (fromEnum to)
      total <- DailyRepository.countDailies
      return (total, dailies)

get :: BusinessId Daily -> AppM (Maybe Daily)
get dailyId = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      DailyRepository.findByDay dailyId


delete :: BusinessId Daily -> AppM (Maybe Daily)
delete dailyId = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      maybeDay <- DailyRepository.findByDay dailyId
      case maybeDay of
        Nothing -> return maybeDay
        Just daily -> do
           DailyRepository.conditionallyDelete daily
           return maybeDay
--      DailyRepository.deleteDaily dailyId
--      return maybeDay

getAllForMonth :: BusinessId Company -> BusinessId Customer -> SpecificMonth -> AppM [Daily]
getAllForMonth companyId customerId (SpecificMonth y m) = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      DailyRepository.workPacksForMonth customerId companyId y m

--TODO no diff anymore between daily and newDay. Keep for future or remove?
insert :: NewDaily -> AppM Daily
insert (NewDaily d newWps customId compId) = do
  pool <- asks poel
  uuid <- liftIO UUID.nextRandom
  wps <-
    mapM
      ( \(NewWorkPack a w d) -> do
          uuid <- liftIO UUID.nextRandom
          return $ WorkPack (BusinessId uuid) a w d
      )
      newWps
  let daily = Daily (BusinessId uuid) d wps customId compId False
  void $
    DB.executeInPool pool $
      do
        DailyRepository.insertDaily daily

  return daily
