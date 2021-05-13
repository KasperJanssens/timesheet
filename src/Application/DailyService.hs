module Application.DailyService where

import           Application.Environment
import           Control.Monad                           (void)
import           Control.Monad.Logger                    (runStderrLoggingT)
import           Control.Monad.Trans.Reader              (asks)
import           Data.Text                               (Text)
import           Data.Time                               (Day)
import           Data.UUID                               (UUID)
import qualified Data.UUID.V4 as UUID
import           Database.Persist.Postgresql             (ConnectionString,
                                                          withPostgresqlPool)
import           Domain.Daily                            (Daily (..))
import           Domain.Monthly                          (SpecificMonth (..))
import           ExternalAPI.NewTypes.NewDaily           (NewDaily (..))
import qualified InternalAPI.Persistence.DailyRepository as DailyRepository
import qualified InternalAPI.Persistence.Database        as DB
import           Numeric.Natural                         (Natural)
import Control.Monad.IO.Class (liftIO)

list :: Natural -> Natural -> AppM (Int, [Daily])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      dailies <- DailyRepository.getDailies (fromEnum from) (fromEnum to)
      total <- DailyRepository.countDailies
      return (total, dailies)

get :: UUID -> AppM (Maybe Daily)
get dailyId = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      DailyRepository.findByDay dailyId

delete :: UUID -> AppM (Maybe Daily)
delete dailyId = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      maybeDay <- DailyRepository.findByDay dailyId
      DailyRepository.deleteDaily dailyId
      return maybeDay

getAllForMonth :: UUID -> Text -> SpecificMonth -> AppM [Daily]
getAllForMonth companyId customerVat (SpecificMonth y m) = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      DailyRepository.workPacksForMonth companyId customerVat y m

--TODO no diff anymore between daily and newDay. Keep for future or remove?
insert :: NewDaily -> AppM Daily
insert (NewDaily d wps cId cVat) = do
  pool <- asks poel
  uuid <- liftIO UUID.nextRandom
  let daily = Daily uuid d wps cId cVat
  void $
    DB.executeInPool pool $
      do
        DailyRepository.insertDaily daily

  return daily
