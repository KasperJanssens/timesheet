{-# LANGUAGE OverloadedStrings #-}

module Application.HourlyRateUseCaseSpec where

import qualified Application.CompanyService       as CompanyService
import qualified Application.CustomerService      as CustomerService
import qualified Application.DailyService         as DailyService
import qualified Application.InvoiceService       as InvoiceService
import           Application.MonthlyService       (UninvoicedWork (..))
import qualified Application.MonthlyService       as MonthlyService
import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Either                      (isRight)
import           Data.Time                        (fromGregorian)
import qualified Data.UUID.V4                     as UUID
import           Domain.Company                   (Company (..))
import qualified Domain.Company                   as Company
import           Domain.Customer                  (VATNumber (..))
import qualified Domain.Customer                  as Customer
import           Domain.Daily
import qualified Domain.Invoice                   as Invoice
import qualified Domain.MonthlyReport             as MonthlyReport
import           ExternalAPI.NewTypes.NewCompany
import qualified ExternalAPI.NewTypes.NewCompany  as NewCompany
import           ExternalAPI.NewTypes.NewCustomer (NewCustomer (..))
import qualified ExternalAPI.NewTypes.NewCustomer as NewCustomer
import           ExternalAPI.NewTypes.NewDaily
import           ExternalAPI.NewTypes.NewInvoice
import           Helper.DatabaseHelper
import           Helper.TestHelper
import           Test.Hspec

fromUninvoiced :: UninvoicedWork -> NewInvoice
fromUninvoiced (UninvoicedWork specificMonth customer company) = NewInvoice specificMonth (Customer.id customer) (Company.vatNumber company)

spec :: Spec
spec = around withDatabase $
  describe "Hourly Rate Use Case Spec" $ do
    it "three different months, single company, single customer" $ \connString -> do
      state <- createInitialState connString
      res <- runAppM state $ do
        company <- CompanyService.insert NewCompany.dummy
        customer <- CustomerService.insert NewCustomer.dummy
        let customerId = Customer.id customer
        let companyVat = Company.vatNumber company
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 5 1) [NewWorkPack 100 FUNCDESI "Functional Design"] customerId companyVat
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 4 1) [NewWorkPack 150 TECHDESI "Technical Design"] customerId companyVat
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 3 1) [NewWorkPack 200 MEET "Meetings meetings"] customerId companyVat

        uninvoicedWork1 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork1 `shouldSatisfy` hasSize 3
        let firstUninvoicedWork = head uninvoicedWork1
        invoice1 <- InvoiceService.insert (fromUninvoiced firstUninvoicedWork)
        let aantalUur1 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice1
        liftIO $ aantalUur1 `shouldBe` 12.5
        expectedInvoiceId1 <- liftIO $ createExpectedInvoiceId "001"
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice1) `shouldBe` expectedInvoiceId1

        uninvoicedWork2 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork2 `shouldSatisfy` hasSize 2

        let secondUninvoicedWork = head uninvoicedWork2
        invoice2 <- InvoiceService.insert (fromUninvoiced secondUninvoicedWork)
        let aantalUur2 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice2
        liftIO $ aantalUur2 `shouldBe` 18.75
        expectedInvoiceId2 <- liftIO $ createExpectedInvoiceId "002"
        liftIO $ print invoice2
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice2) `shouldBe` expectedInvoiceId2

        uninvoicedWork3 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork3 `shouldSatisfy` hasSize 1

        let thirdUninvoicedWork = head uninvoicedWork3
        invoice3 <- InvoiceService.insert (fromUninvoiced thirdUninvoicedWork)
        let aantalUur3 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice3
        liftIO $ aantalUur3 `shouldBe` 25
        expectedInvoiceId3 <- liftIO $ createExpectedInvoiceId "003"
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice3) `shouldBe` expectedInvoiceId3

        uninvoicedWork4 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork4 `shouldSatisfy` null
      res `shouldSatisfy` isRight
    it "three different months, two companies, single customer" $ \connString -> do
      state <- createInitialState connString
      res <- runAppM state $ do
        propellant <- CompanyService.insert NewCompany.dummy
        krondorSoft <- CompanyService.insert (NewCompany "KrondorSoft" "BE0893815606" "OGS 354" "Ievrs op een bank" Nothing Nothing)
        customer <- CustomerService.insert NewCustomer.dummy
        let customerId = Customer.id customer
        let propellantVat = Company.vatNumber propellant
        let krondorSoftVat = Company.vatNumber krondorSoft
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 5 1) [NewWorkPack 100 FUNCDESI "Functional Design"] customerId propellantVat
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 4 1) [NewWorkPack 150 TECHDESI "Technical Design"] customerId krondorSoftVat
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 3 1) [NewWorkPack 200 MEET "Meetings meetings"] customerId propellantVat

        uninvoicedWork1 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork1 `shouldSatisfy` hasSize 3
        let firstUninvoicedWork = head uninvoicedWork1
        invoice1 <- InvoiceService.insert (fromUninvoiced firstUninvoicedWork)
        let aantalUur1 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice1
        liftIO $ aantalUur1 `shouldBe` 12.5
        expectedInvoiceId1 <- liftIO $ createExpectedInvoiceId "001"
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice1) `shouldBe` expectedInvoiceId1

        uninvoicedWork2 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork2 `shouldSatisfy` hasSize 2

        let secondUninvoicedWork = head uninvoicedWork2
        invoice2 <- InvoiceService.insert (fromUninvoiced secondUninvoicedWork)
        let aantalUur2 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice2
        liftIO $ aantalUur2 `shouldBe` 18.75
        expectedInvoiceId2 <- liftIO $ createExpectedInvoiceId "001"
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice2) `shouldBe` expectedInvoiceId2

        uninvoicedWork3 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork3 `shouldSatisfy` hasSize 1

        let thirdUninvoicedWork = head uninvoicedWork3
        invoice3 <- InvoiceService.insert (fromUninvoiced thirdUninvoicedWork)
        let aantalUur3 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice3
        liftIO $ aantalUur3 `shouldBe` 25
        expectedInvoiceId3 <- liftIO $ createExpectedInvoiceId "002"
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice3) `shouldBe` expectedInvoiceId3

        uninvoicedWork4 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork4 `shouldSatisfy` null
      res `shouldSatisfy` isRight
    it "three different months, two companies, two customers" $ \connString -> do
      state <- createInitialState connString
      res <- runAppM state $ do
        propellant <- CompanyService.insert NewCompany.dummy
        krondorSoft <- CompanyService.insert (NewCompany "KrondorSoft" "BE0893815606" "OGS 354" "Ievrs op een bank" Nothing Nothing)
        customer1 <- CustomerService.insert NewCustomer.dummy
        customer2 <- CustomerService.insert (NewCustomer "KrondorSoftbis" (VATNumber "BE0893815607") (Just 75.0) 30)
        let customerId1 = Customer.id customer1
        let customerId2 = Customer.id customer2
        let propellantVat = Company.vatNumber propellant
        let krondorSoftVat = Company.vatNumber krondorSoft
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 5 1) [NewWorkPack 100 FUNCDESI "Functional Design"] customerId1 propellantVat
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 5 1) [NewWorkPack 150 TECHDESI "Technical Design"] customerId1 krondorSoftVat
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 5 1) [NewWorkPack 200 MEET "Meetings meetings"] customerId2 propellantVat

        uninvoicedWork1 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork1 `shouldSatisfy` hasSize 3
        let firstUninvoicedWork = head uninvoicedWork1
        invoice1 <- InvoiceService.insert (fromUninvoiced firstUninvoicedWork)
        let aantalUur1 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice1
        liftIO $ aantalUur1 `shouldBe` 12.5
        expectedInvoiceId1 <- liftIO $ createExpectedInvoiceId "001"
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice1) `shouldBe` expectedInvoiceId1

        uninvoicedWork2 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork2 `shouldSatisfy` hasSize 2

        let secondUninvoicedWork = head uninvoicedWork2
        invoice2 <- InvoiceService.insert (fromUninvoiced secondUninvoicedWork)
        let aantalUur2 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice2
        liftIO $ aantalUur2 `shouldBe` 18.75
        expectedInvoiceId2 <- liftIO $ createExpectedInvoiceId "001"
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice2) `shouldBe` expectedInvoiceId2

        uninvoicedWork3 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork3 `shouldSatisfy` hasSize 1

        let thirdUninvoicedWork = head uninvoicedWork3
        invoice3 <- InvoiceService.insert (fromUninvoiced thirdUninvoicedWork)
        let aantalUur3 = MonthlyReport.totalDays $ Invoice.monthlyReport invoice3
        liftIO $ aantalUur3 `shouldBe` 25
        expectedInvoiceId3 <- liftIO $ createExpectedInvoiceId "002"
        liftIO $ MonthlyReport.invoiceNumber (Invoice.monthlyReport invoice3) `shouldBe` expectedInvoiceId3

        uninvoicedWork4 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork4 `shouldSatisfy` null
      res `shouldSatisfy` isRight