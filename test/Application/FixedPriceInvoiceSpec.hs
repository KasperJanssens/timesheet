{-# LANGUAGE OverloadedStrings #-}

module Application.FixedPriceInvoiceSpec where

import qualified Application.CompanyService                as CompanyService
import qualified Application.CustomerService               as CustomerService
import qualified Application.FixedPriceInvoiceService      as FixedPriceInvoiceService
import qualified Application.QuoteService                  as QuoteService
import           Control.Monad.Cont                        (liftIO)
import           Data.Either                               (fromRight, isRight)
import           Data.Maybe                                (fromJust)
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Data.Text.Internal.Builder                (toLazyText)
import           Data.Text.Lazy                            (toStrict)
import           Data.Text.Lazy.Builder.Int                (decimal)
import           Data.Time.Calendar                        (addDays,
                                                            showGregorian)
import           Data.Time.Calendar.OrdinalDate            (toOrdinalDate)
import           Data.Time.Clock                           (utctDay)
import           Data.Time.Clock.POSIX                     (getCurrentTime)
import qualified Domain.Company                            as Company
import qualified Domain.Customer                           as Customer
import qualified Domain.FixedPriceInvoice                  as FixedPriceInvoice
import           Domain.MonthlyReport                      (totalExcl)
import qualified Domain.Quote                              as Quote
import qualified ExternalAPI.NewTypes.NewCustomer          as NewCustomer
import           ExternalAPI.NewTypes.NewFixedPriceInvoice
import           ExternalAPI.NewTypes.NewQuote             (NewQuote (..))
import           Helper.DatabaseHelper
import           Test.Hspec

createExpectedInvoiceId :: IO Text
createExpectedInvoiceId = do
  today <- getCurrentTime
  let curYear = fromInteger . fst . toOrdinalDate . utctDay $ today
  let curYearText = toStrict . toLazyText . decimal $ curYear
  return $ Text.concat [curYearText, "001"]

createExpectedPaymentDay :: IO Text
createExpectedPaymentDay = do
  today <- getCurrentTime
  let invoiceDate = addDays 30 $ utctDay today
  return $ Text.pack $ showGregorian invoiceDate

spec :: Spec
spec = around withDatabase $
  describe "Fixed price invoice" $ do
    it "should create a correct invoice from price" $ \connString -> do
      state <- createInitialState connString
      fixedPriceInvoiceOrErr <- runAppM state $ do
        res <- FixedPriceInvoiceService.list 0 10
        liftIO $ fst res `shouldBe` 0
        customer <- CustomerService.insert NewCustomer.dummy
        company <- CompanyService.insert Company.dummy
        let newFixedPriceInvoice = NewFixedPriceInvoice 200.0 (Customer.id customer) (Company.vatNumber company)
        fixedPriceInvoice <- FixedPriceInvoiceService.insert newFixedPriceInvoice
        FixedPriceInvoiceService.get (FixedPriceInvoice.id fixedPriceInvoice)
      fixedPriceInvoiceOrErr `shouldSatisfy` isRight
      let fixedPriceInvoice = fromJust $ fromRight undefined fixedPriceInvoiceOrErr
      expectedInvoiceId <- createExpectedInvoiceId
      FixedPriceInvoice.invoiceId fixedPriceInvoice `shouldBe` expectedInvoiceId
      expectedPaymentDay <- createExpectedPaymentDay
      FixedPriceInvoice.dayOfPayment fixedPriceInvoice `shouldBe` expectedPaymentDay
    it "should create a correct invoice from quote" $ \connString -> do
      state <- createInitialState connString
      let price = 100
      invoiceOrErr <- runAppM state $ do
        customer <- CustomerService.insert NewCustomer.dummy
        company <- CompanyService.insert Company.dummy
        quote <- QuoteService.insert (NewQuote price (Customer.id customer) (Company.vatNumber company))
        fixedPriceInvoice <- FixedPriceInvoiceService.insertFromQuote (Quote.id quote)
        FixedPriceInvoiceService.get (FixedPriceInvoice.id fixedPriceInvoice)
      invoiceOrErr `shouldSatisfy` isRight
      let invoice = fromJust $ fromRight undefined invoiceOrErr
      totalExcl (FixedPriceInvoice.vatReport invoice) `shouldBe` price
      Company.name (FixedPriceInvoice.company invoice) `shouldBe` "Propellant"
      Customer.name (FixedPriceInvoice.customer invoice) `shouldBe` "KrondorSoft"
