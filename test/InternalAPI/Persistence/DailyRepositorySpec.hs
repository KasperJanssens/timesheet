{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}

module InternalAPI.Persistence.DailyRepositorySpec where

import           Data.Maybe                          (isJust)
import           Data.Time                           (getCurrentTime, utctDay)
import           Domain.Daily
import qualified InternalAPI.Persistence.DailyRepository as DailyRecord
import           Test.Hspec

import           Helper.DatabaseHelper

spec :: Spec
spec = around withDatabase $  describe "Test daily records in database" $ do
  it "Should list, insert, read, list, delete, get" $ \connString -> do
    dailiesBefore <- runWithoutPool connString $ DailyRecord.getDailies 0 10
    time <- getCurrentTime
    let day = utctDay time
    let daily = Daily day []
    recordId <- runWithoutPool connString $ DailyRecord.insertDaily daily
    maybeRes <- runWithoutPool connString $ DailyRecord.findByDay day

    maybeRes `shouldSatisfy` isJust
    maybeRes `shouldBe` Just daily

    dailiesAfterInsert <- runWithoutPool connString $ DailyRecord.getDailies 0 10

    length dailiesAfterInsert `shouldBe` length dailiesBefore + 1

    runWithoutPool connString $ DailyRecord.deleteDaily day

    dailiesAfterDelete <- runWithoutPool connString $ DailyRecord.getDailies 0 10

    length dailiesAfterDelete `shouldBe` length dailiesBefore

    shouldBeDeleted <- runWithoutPool connString $ DailyRecord.findByDay day

    shouldBeDeleted `shouldSatisfy` null
