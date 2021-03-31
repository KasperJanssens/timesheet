{-# LANGUAGE OverloadedStrings #-}

module ExternalAPI.MonthlyReportSpec where

import           Domain.Daily
import           Domain.MonthlyReport
import           Test.Hspec

spec :: Spec
spec = describe "Monthly report spec" $
  it "Should have working grouping" $ do
    let workpacks = [WorkPack 7.0 TEAM "Meeting", WorkPack 8.0 TEAM "Meeting"]
    let reportEntries = createEntries workpacks
    reportEntries `shouldSatisfy` (\l -> length l == 1)
    let reportEntry = head reportEntries
    omschrijving reportEntry `shouldBe` "TEAM Meeting"
    aantalUur reportEntry `shouldBe` 15.0

