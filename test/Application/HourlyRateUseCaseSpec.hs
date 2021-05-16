{-# LANGUAGE OverloadedStrings #-}

module Application.HourlyRateUseCaseSpec where

import qualified Application.CompanyService       as CompanyService
import qualified Application.CustomerService      as CustomerService
import qualified Application.DailyService         as DailyService
import qualified Application.InvoiceService       as InvoiceService
import           Application.MonthlyService       (UninvoicedWork(..))
import qualified Application.MonthlyService       as MonthlyService
import           Control.Monad                    (void)
import           Data.Either                      (fromRight, isRight)
import           Data.Time                        (fromGregorian)
import qualified Domain.Company                   as Company
import qualified Domain.Customer                  as Customer
import           Domain.Daily
import qualified ExternalAPI.NewTypes.NewCustomer as NewCustomer
import           ExternalAPI.NewTypes.NewDaily
import           ExternalAPI.NewTypes.NewInvoice
import           Helper.DatabaseHelper
import           Test.Hspec
import qualified Data.UUID.V4 as UUID
import Control.Monad.IO.Class (liftIO)

fromUninvoiced :: UninvoicedWork -> NewInvoice
fromUninvoiced (UninvoicedWork specificMonth customer company ) = NewInvoice specificMonth (Customer.id customer) (Company.vatNumber company)

spec :: Spec
spec = around withDatabase $
  describe "Hourly Rate Use Case Spec" $ do
    it "three different months, single company, single customer" $ \connString -> do
      state <- createInitialState connString
      res <- runAppM state $ do
        company <- CompanyService.insert Company.dummy
        customer <- CustomerService.insert NewCustomer.dummy
        let customerId = Customer.id customer
        let companyVat = Company.vatNumber company
        wp1 <- liftIO UUID.nextRandom
        wp2 <- liftIO UUID.nextRandom
        wp3 <- liftIO UUID.nextRandom
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 5 1) [WorkPack wp1 100 FUNCDESI "Functional Design"] customerId companyVat
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 4 1) [WorkPack wp2 150 TECHDESI "Technical Design"] customerId companyVat
        void $ DailyService.insert $ NewDaily (fromGregorian 2021 3 1) [WorkPack wp3 200 MEET "Meetings meetings"] customerId companyVat

        uninvoicedWork1 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork1 `shouldSatisfy` hasSize 3
        let firstUninvoicedWork = head uninvoicedWork1
        invoice1 <- InvoiceService.insert (fromUninvoiced firstUninvoicedWork)

        uninvoicedWork2 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork2 `shouldSatisfy` hasSize 2
        
        let secondUninvoicedWork = head uninvoicedWork2
        invoice2 <- InvoiceService.insert (fromUninvoiced secondUninvoicedWork)
        uninvoicedWork3 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork3 `shouldSatisfy` hasSize 1
        
        let thirdUninvoicedWork = head uninvoicedWork3
        invoice3 <- InvoiceService.insert (fromUninvoiced thirdUninvoicedWork)
        uninvoicedWork4 <- MonthlyService.selectMonthsWithUninvoicedWork
        liftIO $ uninvoicedWork4 `shouldSatisfy` null
                

      res `shouldSatisfy` isRight
