{-# LANGUAGE OverloadedStrings #-}

module Application.InvoiceServiceSpec where

import qualified Application.CustomerService      as CustomerService
import qualified Application.DailyService         as DailyService
import qualified Application.InvoiceService       as InvoiceService
import           Data.Either                      (fromRight, isRight)
import           Domain.Customer
import           Domain.Daily
import qualified Domain.Invoice                   as Invoice
import           Domain.Monthly
import           Domain.MonthlyReport
import           ExternalAPI.NewTypes.NewCustomer
import           ExternalAPI.NewTypes.NewDaily
import           ExternalAPI.NewTypes.NewInvoice
import           Helper.DatabaseHelper
import           Test.Hspec
import Control.Monad.Cont (void)
import Data.Time.Calendar (fromGregorian)



spec :: Spec
spec = around withDatabase $ describe "invoiceService" $ do
  it "should find an invoice, without monthly report" $ \connString -> do
    state <- createInitialState connString
    invoiceOrErr <- runAppM state $ do
      customer <- CustomerService.insert (NewCustomer "Jos" (VATNumber "een nummer") (Just 75.0) 30)
      let customerId = Domain.Customer.id customer
      InvoiceService.insert (NewInvoice (SpecificMonth 2021 5) customerId)
    invoiceOrErr `shouldSatisfy` isRight
    let invoice = fromRight undefined invoiceOrErr
    Invoice.id invoice `shouldBe` SpecificMonth 2021 5
    Domain.Customer.name (Invoice.customer invoice) `shouldBe` "Jos"
    totalDays (Invoice.monthlyReport invoice) `shouldBe` 0.0
    reportEntries (Invoice.monthlyReport invoice) `shouldSatisfy` null
    vatReport (Invoice.monthlyReport invoice) `shouldBe` VATReport 0.0 0.0 0.0
  it "should find an invoice, with monthly report" $ \connString -> do
    state <- createInitialState connString
    invoiceOrErr <- runAppM state $ do
      void $ DailyService.insert (NewDaily (fromGregorian 2021 5 2) [WorkPack 7.0 IMPL "Jos" ])
      void $ DailyService.insert (NewDaily (fromGregorian 2021 5 3) [WorkPack 5.0 IMPL "Jos" ])
      void $ DailyService.insert (NewDaily (fromGregorian 2021 5 4) [WorkPack 6.0 IMPL "Jos" ])
      void $ DailyService.insert (NewDaily (fromGregorian 2021 5 5) [WorkPack 6.0 FUNCDESI "Smos" ])
      customer <- CustomerService.insert (NewCustomer "Jos" (VATNumber "een nummer") (Just 75.0) 30)
      let customerId = Domain.Customer.id customer
      InvoiceService.insert (NewInvoice (SpecificMonth 2021 5) customerId)
    invoiceOrErr `shouldSatisfy` isRight
    let invoice = fromRight undefined invoiceOrErr
    Invoice.id invoice `shouldBe` SpecificMonth 2021 5
    Domain.Customer.name (Invoice.customer invoice) `shouldBe` "Jos"
    totalDays (Invoice.monthlyReport invoice) `shouldBe` 3.0
    reportEntries (Invoice.monthlyReport invoice) `shouldSatisfy` hasSize 2
    vatReport (Invoice.monthlyReport invoice) `shouldBe` VATReport 1800.0 378.0 2178.0


