module Application.DailyService where

import Application.Environment
import Control.Monad (void)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (asks)
import Data.Time (Day)
import Database.Persist.Postgresql
  ( ConnectionString,
    withPostgresqlPool,
  )
import Domain.Daily (Daily (..))
import Domain.Monthly (SpecificMonth (..))
import ExternalAPI.NewTypes.NewDaily (NewDaily (..))
import qualified InternalAPI.Persistence.DailyRepository as DailyRepository
import qualified InternalAPI.Persistence.Database as DB
import Numeric.Natural (Natural)

list :: Natural -> Natural -> AppM (Int, [Daily])
list from to = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      dailies <- DailyRepository.getDailies (fromEnum from) (fromEnum to)
      total <- DailyRepository.countDailies
      return (total, dailies)

get :: Day -> AppM (Maybe Daily)
get day = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      DailyRepository.findByDay day

delete :: Day -> AppM (Maybe Daily)
delete day = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      maybeDay <- DailyRepository.findByDay day
      DailyRepository.deleteDaily day
      return maybeDay

getAllForMonth :: SpecificMonth -> AppM [Daily]
getAllForMonth (SpecificMonth y m) = do
  pool <- asks poel
  DB.executeInPool pool $
    do
      DailyRepository.workPacksForMonth y m

--TODO no diff anymore between daily and newDay. Keep for future or remove?
insert :: NewDaily -> AppM Daily
insert (NewDaily d wps) = do
  pool <- asks poel
  let daily = Daily d wps
  void $
    DB.executeInPool pool $
      do
        DailyRepository.insertDaily daily

  return daily
