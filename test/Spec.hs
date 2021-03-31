import qualified ExternalAPI.MonthlyReportSpec                  as MonthlyReportSpec
import qualified InternalAPI.Persistence.CustomerRepositorySpec as CustomerRecordSpec
import qualified InternalAPI.Persistence.DailyRepositorySpec    as DailyRecordSpec
import           Test.Hspec
import qualified Application.InvoiceServiceSpec as InvoiceServiceSpec

main :: IO ()
main = hspec $ do
  MonthlyReportSpec.spec
  DailyRecordSpec.spec
  CustomerRecordSpec.spec
  InvoiceServiceSpec.spec
