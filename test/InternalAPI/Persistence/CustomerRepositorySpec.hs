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
import qualified InternalAPI.Persistence.CustomerRepository as CustomerRecord
import           Test.Hspec

spec :: Spec
spec = around withDatabase $
  describe "Test daily records in database" $ do
    it "Should list, insert, read, list, delete, get" $ \connString -> do
      dailiesBefore <- runWithoutPool connString $ CustomerRecord.getCustomers 0 10
      businessId <- UUID.nextRandom
      let customer = Customer businessId "jos" (VATNumber "BEsomethingsomething") (Just 75.0) 30
      recordId <- runWithoutPool connString $ CustomerRecord.insertCustomer customer
      --    TODO this is not great, the packaging in business id should be avoided
      maybeRes <- runWithoutPool connString $ CustomerRecord.findByBusinessId $ BusinessId businessId

      maybeRes `shouldSatisfy` isJust
      maybeRes `shouldBe` Just customer

      dailiesAfterInsert <- runWithoutPool connString $ CustomerRecord.getCustomers 0 10

      length dailiesAfterInsert `shouldBe` length dailiesBefore + 1

      runWithoutPool connString $ CustomerRecord.deleteCustomer $ BusinessId businessId

      dailiesAfterDelete <- runWithoutPool connString $ CustomerRecord.getCustomers 0 10

      length dailiesAfterDelete `shouldBe` length dailiesBefore

      shouldBeDeleted <- runWithoutPool connString $ CustomerRecord.findByBusinessId $ BusinessId businessId

      shouldBeDeleted `shouldSatisfy` null
