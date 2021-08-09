import qualified Application.FixedPriceInvoiceSpec              as FixedPriceInvoiceSpec
import qualified Application.HourlyRateUseCaseSpec              as HourlyRateUseCaseSpec
import qualified Application.InvoiceServiceSpec                 as InvoiceServiceSpec
import qualified Application.QuoteServiceSpec                   as QuoteServiceSpec
import qualified Application.QuoteUseCaseSpec                   as QuoteUseCaseSpec
import qualified ExternalAPI.MonthlyReportSpec                  as MonthlyReportSpec
import qualified InternalAPI.Persistence.CompanyRepositorySpec  as CompanyRepositorySpec
import qualified InternalAPI.Persistence.CustomerRepositorySpec as CustomerRecordSpec
import qualified InternalAPI.Persistence.DailyRepositorySpec    as DailyRecordSpec
import qualified Domain.VATSpec as VATSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  MonthlyReportSpec.spec
  DailyRecordSpec.spec
  CustomerRecordSpec.spec
  InvoiceServiceSpec.spec
  FixedPriceInvoiceSpec.spec
  QuoteServiceSpec.spec
  QuoteUseCaseSpec.spec
  HourlyRateUseCaseSpec.spec
  CompanyRepositorySpec.unitSpec
  VATSpec.spec
