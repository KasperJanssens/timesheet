{-# LANGUAGE OverloadedStrings #-}

module Application.FixedPriceInvoiceSpec where

import qualified Application.CompanyService                as CompanyService
import qualified Application.CustomerService               as CustomerService
import qualified Application.FixedPriceInvoiceService      as FixedPriceInvoiceService
import qualified Application.QuoteService                  as QuoteService
import           Control.Monad.Cont                        (liftIO)
import           Data.Either                               (fromRight, isRight)
import           Data.Maybe                                (fromJust)
import           Data.Time                                 (getCurrentTime,
                                                            utctDay)
import qualified Domain.Company                            as Company
import qualified Domain.Customer                           as Customer
import qualified Domain.FixedPriceInvoice                  as FixedPriceInvoice
import           Domain.MonthlyReport                      (totalExcl)
import qualified Domain.Quote                              as Quote
import qualified ExternalAPI.NewTypes.NewCompany           as NewCompany
import qualified ExternalAPI.NewTypes.NewCustomer          as NewCustomer
import           ExternalAPI.NewTypes.NewFixedPriceInvoice
import           ExternalAPI.NewTypes.NewQuote             (NewQuote (..))
import           Helper.DatabaseHelper
import           Helper.TestHelper
import           Test.Hspec

spec :: Spec
spec = around withDatabase $
  describe "Fixed price invoice" $ do
    it "should create a correct invoice from price" $ \connString -> do
      state <- createInitialState connString
      fixedPriceInvoiceOrErr <- runAppM state $ do
        time <- liftIO getCurrentTime
        let today = utctDay time
        res <- FixedPriceInvoiceService.list 0 10
        liftIO $ fst res `shouldBe` 0
        customer <- CustomerService.insert NewCustomer.dummy
        company <- CompanyService.insert NewCompany.dummy
        let newFixedPriceInvoice = NewFixedPriceInvoice (Left (NonQuote 200.0 (Customer.id customer) (Company.id company) "zever")) today
        fixedPriceInvoice <- FixedPriceInvoiceService.insert newFixedPriceInvoice
        FixedPriceInvoiceService.get (FixedPriceInvoice.id fixedPriceInvoice)
      fixedPriceInvoiceOrErr `shouldSatisfy` isRight
      let fixedPriceInvoice = fromJust $ fromRight undefined fixedPriceInvoiceOrErr
      expectedInvoiceId <- createExpectedInvoiceId "001"
      FixedPriceInvoice.invoiceId fixedPriceInvoice `shouldBe` expectedInvoiceId
      expectedPaymentDay <- createExpectedPaymentDay
      FixedPriceInvoice.dayOfPayment fixedPriceInvoice `shouldBe` expectedPaymentDay
    it "should create a correct invoice from quote" $ \connString -> do
      state <- createInitialState connString
      let price = 100
      invoiceOrErr <- runAppM state $ do
        time <- liftIO getCurrentTime
        let today = utctDay time
        customer <- CustomerService.insert NewCustomer.dummy
        company <- CompanyService.insert NewCompany.dummy
        quote <- QuoteService.insert (NewQuote price (Customer.id customer) (Company.id company) "zever" "vree rap")
        fixedPriceInvoice <- FixedPriceInvoiceService.insertFromQuote (Quote.id quote) today
        FixedPriceInvoiceService.get (FixedPriceInvoice.id fixedPriceInvoice)
      invoiceOrErr `shouldSatisfy` isRight
      let invoice = fromJust $ fromRight undefined invoiceOrErr
      totalExcl (FixedPriceInvoice.vatReport invoice) `shouldBe` price
      Company.name (FixedPriceInvoice.company invoice) `shouldBe` "Propellant"
      Customer.name (FixedPriceInvoice.customer invoice) `shouldBe` "KrondorSoft"
