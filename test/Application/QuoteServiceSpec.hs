{-# LANGUAGE OverloadedStrings #-}

module Application.QuoteServiceSpec where

import qualified Application.CompanyService       as CompanyService
import qualified Application.CustomerService      as CustomerService
import qualified Application.QuoteService         as QuoteService
import           Control.Monad.Cont               (liftIO)
import           Data.Either                      (fromRight, isRight)
import           Data.Maybe                       (fromJust)
import qualified Domain.Company                   as Company
import qualified Domain.Customer                  as Customer
import           Domain.MonthlyReport
import qualified Domain.Quote                     as Quote
import qualified ExternalAPI.NewTypes.NewCustomer as NewCustomer
import qualified ExternalAPI.NewTypes.NewCompany as NewCompany
import           ExternalAPI.NewTypes.NewQuote
import           Helper.DatabaseHelper
import           Test.Hspec

spec :: Spec
spec = around withDatabase $
  describe "quote service spec" $ do
    it "should insert a quote" $ \connString -> do
      state <- createInitialState connString
      let quoteVal = 100
      quoteRes <- runAppM state $ do
        company <- CompanyService.insert NewCompany.dummy
        customer <- CustomerService.insert NewCustomer.dummy
        listResult <- QuoteService.list 0 10
        liftIO $ fst listResult `shouldBe` 0
        liftIO $ snd listResult `shouldSatisfy` null
        quote <- QuoteService.insert (NewQuote quoteVal (Customer.id customer) (Company.vatNumber company))
        QuoteService.get (Quote.id quote)
      quoteRes `shouldSatisfy` isRight
      let quote = fromJust . fromRight undefined $ quoteRes
      Company.name (Quote.company quote) `shouldBe` "Propellant"
      Customer.name (Quote.customer quote) `shouldBe` "KrondorSoft"
      totalExcl (Quote.vatReport quote) `shouldBe` quoteVal
