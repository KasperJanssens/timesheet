module Application.MonthlyService where

import           Application.Environment                 (AppM, MonthlyMap,
                                                          poel)
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Trans.Reader              (asks)
import qualified Data.Map                                as Map
import           Data.Text                               (Text)
import           Data.Time                               (getCurrentTime,
                                                          utctDay)
import           Data.Time.Calendar.Julian
import           Data.UUID                               (UUID)
import           Debug.Trace
import           Domain.Daily                            (Daily (..))
import           Domain.Monthly                          (Monthly (..),
                                                          SpecificMonth (..))
import qualified Domain.Monthly                          as Monthly
import           Domain.MonthlyReport                    (MonthlyReport,
                                                          toMonthlyReport)
import qualified InternalAPI.Persistence.DailyRepository as DailyRepository
import qualified InternalAPI.Persistence.Database        as DB

updateMonthlyMap :: Daily -> MonthlyMap -> ((), MonthlyMap)
updateMonthlyMap daily@(Daily day _ _ _) map =
  let (year, monthOfYear, _) = toJulian day
   in let updatedMap = Map.insertWith merge (SpecificMonth year monthOfYear) (Monthly.create year monthOfYear daily) map
       in ((), updatedMap)

merge :: Monthly -> Monthly -> Monthly
merge (Monthly i month newValue) (Monthly _ _ oldValue) = Monthly i month $ newValue ++ oldValue

selectMonthsWithAtLeastOneDay :: AppM [SpecificMonth]
selectMonthsWithAtLeastOneDay = do
  pool <- asks poel
  monthYears <- DB.executeInPool pool DailyRepository.allMonthsWithWorkedDays
  return $ (\(m, y) -> SpecificMonth (toInteger y) m) <$> monthYears

getReport :: UUID -> Text -> SpecificMonth -> AppM MonthlyReport
getReport customerId companyVat specificMonth@(SpecificMonth year month) = do
  pool <- asks poel
  dailies <- DB.executeInPool pool $ DailyRepository.workPacksForMonth customerId companyVat year month
  today <- liftIO getCurrentTime
  let monthlyReport = toMonthlyReport (utctDay today) specificMonth dailies
  return monthlyReport
