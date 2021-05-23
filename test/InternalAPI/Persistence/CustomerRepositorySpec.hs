{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module InternalAPI.Persistence.CustomerRepositorySpec where

import           Data.Coerce                                (coerce)
import           Data.Maybe                                 (isJust)
import           Data.Time                                  (getCurrentTime,
                                                             utctDay)
import qualified Data.UUID.V4                               as UUID
import           Domain.Customer
import           Helper.DatabaseHelper
import           InternalAPI.Persistence.BusinessId
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRepository
import           Test.Hspec

spec :: Spec
spec = around withDatabase $
  describe "Test daily records in database" $ do
    it "Should list, insert, read, list, delete, get" $ \connString -> do
      dailiesBefore <- runWithoutPool connString $ CustomerRepository.getCustomers 0 10
      businessId <- UUID.nextRandom
      let customer = Customer businessId "jos" "de straat" "de stad" (VATNumber "BEsomethingsomething")  (Just 75.0) 30
      recordId <- runWithoutPool connString $ CustomerRepository.insertCustomer customer
      --    TODO this is not great, the packaging in business id should be avoided
      maybeRes <- runWithoutPool connString $ CustomerRepository.findByBusinessId $ BusinessId businessId

      maybeRes `shouldSatisfy` isJust
      maybeRes `shouldBe` Just customer

      dailiesAfterInsert <- runWithoutPool connString $ CustomerRepository.getCustomers 0 10

      length dailiesAfterInsert `shouldBe` length dailiesBefore + 1

      runWithoutPool connString $ CustomerRepository.deleteCustomer $ BusinessId businessId

      dailiesAfterDelete <- runWithoutPool connString $ CustomerRepository.getCustomers 0 10

      length dailiesAfterDelete `shouldBe` length dailiesBefore

      shouldBeDeleted <- runWithoutPool connString $ CustomerRepository.findByBusinessId $ BusinessId businessId

      shouldBeDeleted `shouldSatisfy` null
    
