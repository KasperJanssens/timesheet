{-# LANGUAGE OverloadedStrings #-}

module Helper.TestHelper where

import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Internal.Builder     (toLazyText)
import           Data.Text.Lazy                 (toStrict)
import           Data.Text.Lazy.Builder.Int     (decimal)
import           Data.Time                      (addDays, getCurrentTime,
                                                 showGregorian, utctDay)
import           Data.Time.Calendar.OrdinalDate (toOrdinalDate)

createExpectedInvoiceId :: Text -> IO Text
createExpectedInvoiceId invoiceNumber = do
  today <- getCurrentTime
  let curYear = fromInteger . fst . toOrdinalDate . utctDay $ today
  let curYearText = toStrict . toLazyText . decimal $ curYear
  return $ Text.concat [curYearText, invoiceNumber]

createExpectedPaymentDay :: IO Text
createExpectedPaymentDay = do
  today <- getCurrentTime
  let invoiceDate = addDays 30 $ utctDay today
  return $ Text.pack $ showGregorian invoiceDate
