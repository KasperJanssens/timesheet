{-# LANGUAGE OverloadedStrings #-}

module ExternalAPI.MonthlyReportSpec where

import qualified Data.UUID.V4                       as UUID
import           Domain.Daily
import           Domain.MonthlyReport
import           InternalAPI.Persistence.BusinessId
import           Test.Hspec

spec :: Spec
spec = describe "Monthly report spec" $
  it "Should have working grouping" $ do
    uuid1 <- UUID.nextRandom
    uuid2 <- UUID.nextRandom
    let workpacks = [WorkPack  (BusinessId uuid1) 7.0 TEAM "Meeting", WorkPack (BusinessId uuid2) 8.0 TEAM "Meeting"]
    let reportEntries = createEntries workpacks
    reportEntries `shouldSatisfy` (\l -> length l == 1)
    let reportEntry = head reportEntries
    omschrijving reportEntry `shouldBe` "TEAM Meeting"
    aantalUur reportEntry `shouldBe` 15.0

