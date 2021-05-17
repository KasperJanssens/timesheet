module Application.QuoteUseCaseSpec where

import qualified Application.CompanyService           as CompanyService
import qualified Application.CustomerService          as CustomerService
import qualified Application.FixedPriceInvoiceService as FixedPriceInvoiceService
import qualified Application.QuoteService             as QuoteService
import           Control.Monad.Cont                   (void)
import           Control.Monad.IO.Class               (liftIO)
import qualified Domain.Company                       as Company
import qualified Domain.Customer                      as Customer
import qualified Domain.Quote                         as Quote
import qualified ExternalAPI.NewTypes.NewCustomer     as NewCustomer
import qualified ExternalAPI.NewTypes.NewCompany     as NewCompany
import           ExternalAPI.NewTypes.NewQuote
import           Helper.DatabaseHelper
import           Test.Hspec

spec :: Spec
spec = around withDatabase $
  describe "quote use case spec" $ do
    it "list active quotes, invoice one, have one less active quote" $ \connString -> do
      state <- createInitialState connString
      void $
        runAppM state $ do
          customer <- CustomerService.insert NewCustomer.dummy
          company <- CompanyService.insert NewCompany.dummy
          quote1 <- QuoteService.insert (NewQuote 100 (Customer.id customer) (Company.vatNumber company))
          quote2 <- QuoteService.insert (NewQuote 200 (Customer.id customer) (Company.vatNumber company))
          quote3 <- QuoteService.insert (NewQuote 300 (Customer.id customer) (Company.vatNumber company))

          invoiceList1 <- FixedPriceInvoiceService.list 0 10
          liftIO $ fst invoiceList1 `shouldBe` 0
          nonInvoicedList1 <- QuoteService.listNonInvoiced
          liftIO $ nonInvoicedList1 `shouldSatisfy` hasSize 3

          _ <- FixedPriceInvoiceService.insertFromQuote (Quote.id quote1)

          invoiceList2 <- FixedPriceInvoiceService.list 0 10
          liftIO $ fst invoiceList2 `shouldBe` 1
          nonInvoicedList2 <- QuoteService.listNonInvoiced
          liftIO $ nonInvoicedList2 `shouldSatisfy` hasSize 2

          _ <- FixedPriceInvoiceService.insertFromQuote (Quote.id quote2)

          invoiceList3 <- FixedPriceInvoiceService.list 0 10
          liftIO $ fst invoiceList3 `shouldBe` 2
          nonInvoicedList3 <- QuoteService.listNonInvoiced
          liftIO $ nonInvoicedList3 `shouldSatisfy` hasSize 1

          _ <- FixedPriceInvoiceService.insertFromQuote (Quote.id quote3)

          invoiceList4 <- FixedPriceInvoiceService.list 0 10
          liftIO $ fst invoiceList4 `shouldBe` 3
          nonInvoicedList4 <- QuoteService.listNonInvoiced
          liftIO $ nonInvoicedList4 `shouldSatisfy` hasSize 0
