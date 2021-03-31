{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.MonthlyReport where

import           Data.Aeson         (FromJSON, ToJSON)
import qualified Data.List          as List
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Time.Calendar (Day, addDays, showGregorian)
import           Debug.Trace
import           Domain.Daily       (Daily, WorkPack (..), workpacks)
import           Domain.Monthly
import           GHC.Generics       (Generic)
import           Numeric.Natural    (Natural)
import           Safe               (headMay)

standaardTarief :: Double
standaardTarief = 75.0

data VATReport = VATReport {
  totalExcl  :: Double
  , totalVAT :: Double
  , total    :: Double} deriving (FromJSON, ToJSON, Generic, Eq, Show)

data ReportEntry = ReportEntry {
  omschrijving :: Text
  , aantalUur  :: Double
  } deriving (FromJSON, ToJSON, Generic, Eq, Show)

data MonthlyReport = MonthlyReport {
  id              :: SpecificMonth
  , totalDays     :: Double
  , reportEntries :: [ReportEntry]
  , vatReport     :: VATReport
  , month         :: Text
  , invoiceNumber :: Text
  , dayOfInvoice  :: Text
  , dayOfPayment  :: Text
  } deriving (FromJSON, ToJSON, Generic, Eq, Show)

calculateTotalDays :: [ReportEntry] -> Double
calculateTotalDays reportEntries = List.sum (aantalUur <$> reportEntries) / 8

calculateVATReport :: Double -> VATReport
calculateVATReport totalDays =
  let totalAmountExclVAT = totalDays * 8 * standaardTarief
   in let totalAmount = totalAmountExclVAT * 1.21
       in let totalVATAmount = totalAmount - totalAmountExclVAT
           in VATReport totalAmountExclVAT totalVATAmount totalAmount

createDescription :: WorkPack -> Text
createDescription wp = Text.intercalate " " [Text.pack . show . workType $ wp, description wp]

createEntry :: [WorkPack] -> ReportEntry
createEntry workPacks =
  let totalHours = List.sum $ amount <$> workPacks
   in let subTotal = totalHours * standaardTarief
       in let maybeHead = headMay workPacks
           in let description = maybe "" createDescription maybeHead
               in ReportEntry description totalHours

createEntries :: [WorkPack] -> [ReportEntry]
createEntries workPacks =
  let groupedWorkPacks = List.groupBy (\leftWP rightWP -> workType leftWP == workType rightWP && description leftWP == description rightWP) workPacks
   in trace ("grouped workpacks " ++ show groupedWorkPacks) $ createEntry <$> groupedWorkPacks

toMonthlyReport :: Day -> SpecificMonth -> [Daily] -> MonthlyReport
toMonthlyReport today  specificMonth@(SpecificMonth y  m)  dailies =
  let ws = concat $ workpacks <$> dailies
   in let entries = createEntries ws
       in let totalDays = calculateTotalDays entries
           in let vatReport = calculateVATReport totalDays
               in MonthlyReport
                    specificMonth
                    totalDays
                    entries
                    vatReport
                    (toTextMonth m)
                    "ne skoonen numero"
                    (Text.pack $ showGregorian today)
                    (Text.pack . showGregorian . addDays 30 $ today)
