{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module InternalAPI.Persistence.DailyRepositorySpec where

import qualified Application.CompanyService              as CompanyService
import qualified Application.CustomerService             as CustomerService
import           Data.Either.Combinators                 (fromRight)
import           Data.Maybe                              (isJust)
import           Data.Maybe                              (fromJust)
import           Data.Time                               (getCurrentTime,
                                                          utctDay)
import qualified Data.UUID.V4                            as UUID
import qualified Domain.Company                          as Company
import           Domain.Customer
import qualified Domain.Customer                         as Customer
import           Domain.Daily
import           Domain.VAT                              (maybeCreateBelgianVAT)
import           ExternalAPI.NewTypes.NewCompany
import           ExternalAPI.NewTypes.NewCustomer
import           Helper.DatabaseHelper
import           InternalAPI.Persistence.BusinessId
import qualified InternalAPI.Persistence.DailyRepository as DailyRecord
import           Test.Hspec

spec :: Spec
spec = around withDatabase $
  describe "Test daily records in database" $ do
    it "Should list, insert, read, list, delete, get" $ \connString -> do
      dailiesBefore <- runWithoutPool connString $ DailyRecord.getDailies 0 10
      time <- getCurrentTime
      let day = utctDay time
      initialState <- createInitialState connString
      resOrErr <- runAppM initialState $ do
        customer <- CustomerService.insert (NewCustomer "Jos" (VATNumber "een nummer") "de straat" "de stad" (Just 75.0) 30)
        company <- CompanyService.insert $ NewCompany "Jos het bedrijf" (fromJust $ maybeCreateBelgianVAT 8938156 06)  "hier" "die stad" "de rekening" Nothing Nothing
        return (Customer.id customer, Company.id company)
      let (customerId, companyId) = fromRight undefined resOrErr
      uuid <- UUID.nextRandom
      let dailyBusinessId = BusinessId uuid
      let daily = Daily dailyBusinessId day [] customerId companyId False
      recordId <- runWithoutPool connString $ DailyRecord.insertDaily daily
      maybeRes <- runWithoutPool connString $ DailyRecord.findByDay dailyBusinessId

      maybeRes `shouldSatisfy` isJust
      maybeRes `shouldBe` Just daily

      dailiesAfterInsert <- runWithoutPool connString $ DailyRecord.getDailies 0 10

      length dailiesAfterInsert `shouldBe` length dailiesBefore + 1

      runWithoutPool connString $ DailyRecord.deleteDaily dailyBusinessId

      dailiesAfterDelete <- runWithoutPool connString $ DailyRecord.getDailies 0 10

      length dailiesAfterDelete `shouldBe` length dailiesBefore

      shouldBeDeleted <- runWithoutPool connString $ DailyRecord.findByDay dailyBusinessId

      shouldBeDeleted `shouldSatisfy` null
