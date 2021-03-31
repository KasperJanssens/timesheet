module Application.MonthlyService where

import           Application.Environment                 (AppM, MonthlyMap,
                                                          poel)
import           Control.Monad.Trans.Reader              (asks)
import qualified Data.Map                                as Map
import           Data.Time.Calendar.Julian
import           Debug.Trace
import           Domain.Daily                            (Daily (..))
import           Domain.Monthly                          (Monthly (..),
                                                          SpecificMonth (..))
import qualified Domain.Monthly                          as Monthly
import           Domain.MonthlyReport               (MonthlyReport, toMonthlyReport)
import qualified InternalAPI.Persistence.DailyRepository as DailyRepository
import qualified InternalAPI.Persistence.Database        as DB
import Data.Time (utctDay, getCurrentTime)
import Control.Monad.IO.Class (liftIO)

updateMonthlyMap :: Daily -> MonthlyMap -> ((), MonthlyMap)
updateMonthlyMap daily@(Daily day _) map =
  let (year, monthOfYear, _) = toJulian day
   in let updatedMap = Map.insertWith merge (SpecificMonth year monthOfYear) (Monthly.create year monthOfYear daily) map
       in trace ("map is " ++ show updatedMap) $ ((), updatedMap)

merge :: Monthly -> Monthly -> Monthly
merge (Monthly i month newValue) (Monthly _ _ oldValue) = Monthly i month $ newValue ++ oldValue

selectMonthsWithAtLeastOneDay :: AppM [SpecificMonth]
selectMonthsWithAtLeastOneDay = do
  pool <- asks poel
  monthYears <- DB.executeInPool pool DailyRepository.allMonthsWithWorkedDays
  return $ (\(m, y) -> SpecificMonth (toInteger y) m) <$> monthYears

getReport :: SpecificMonth -> AppM MonthlyReport
getReport specificMonth@(SpecificMonth year month) = do
  pool <- asks poel
  dailies <- DB.executeInPool pool $ DailyRepository.workPacksForMonth year month
  today <- liftIO getCurrentTime
  let monthlyReport = toMonthlyReport (utctDay today) specificMonth dailies
  return monthlyReport
