module Application.MonthlyService where

import           Application.Environment                    (AppM, MonthlyMap,
                                                             poel)
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.Trans.Reader                 (asks)
import qualified Data.Map                                   as Map
import           Data.Text                                  (Text)
import           Data.Time                                  (getCurrentTime,
                                                             utctDay)
import           Data.Time.Calendar.Julian
import           Data.UUID                                  (UUID)
import           Debug.Trace
import           Domain.Company                             (Company)
import           Domain.Customer                            (Customer)
import           Domain.Daily                               (Daily (..))
import           Domain.Monthly                             (Monthly (..),
                                                             SpecificMonth (..))
import qualified Domain.Monthly                             as Monthly
import           Domain.MonthlyReport                       (MonthlyReport,
                                                             toMonthlyReport)
import           InternalAPI.Persistence.BusinessId         (BusinessId)
import           InternalAPI.Persistence.CompanyRepository  as CompanyRepository
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository
import qualified InternalAPI.Persistence.DailyRepository    as DailyRepository
import qualified InternalAPI.Persistence.Database           as DB

updateMonthlyMap :: Daily -> MonthlyMap -> ((), MonthlyMap)
updateMonthlyMap daily@(Daily _ day _ _ _ _) map =
  let (year, monthOfYear, _) = toJulian day
   in let updatedMap = Map.insertWith merge (SpecificMonth (fromIntegral year) monthOfYear) (Monthly.create (fromIntegral year) monthOfYear daily) map
       in ((), updatedMap)

merge :: Monthly -> Monthly -> Monthly
merge (Monthly i month newValue) (Monthly _ _ oldValue) = Monthly i month $ newValue ++ oldValue

data UninvoicedWork = UninvoicedWork SpecificMonth Customer Company deriving (Show, Eq, Ord)

selectMonthsWithUninvoicedWork :: AppM [UninvoicedWork]
selectMonthsWithUninvoicedWork = do
  pool <- asks poel
  uninvoicedWork <- DB.executeInPool pool DailyRepository.allMonthsWithWorkedDays
  return $ (\(m, y, customer, company) -> UninvoicedWork (SpecificMonth  y m) (CustomerRepository.to customer) (CompanyRepository.to company)) <$> uninvoicedWork

getReport :: BusinessId Customer -> BusinessId Company -> SpecificMonth -> AppM MonthlyReport
getReport customerId companyId specificMonth@(SpecificMonth year month) = do
  pool <- asks poel
  dailies <- DB.executeInPool pool $ DailyRepository.workPacksForMonth customerId companyId year month
  today <- liftIO getCurrentTime
  let monthlyReport = toMonthlyReport (utctDay today) specificMonth dailies
  return monthlyReport
