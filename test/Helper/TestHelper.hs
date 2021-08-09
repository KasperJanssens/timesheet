{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Helper.TestHelper where

import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Internal.Builder     (toLazyText)
import           Data.Text.Lazy                 (toStrict)
import           Data.Text.Lazy.Builder.Int     (decimal)
import           Data.Time                      (addDays, getCurrentTime,
                                                 showGregorian, utctDay)
import           Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import GHC.Stack
--import Control.Monad.Catch
import Control.Exception.Safe



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

data ShowEqBox = forall s. (Show s, Eq s) => SEB s
instance Show ShowEqBox where show (SEB x) = show x
instance Eq ShowEqBox where (SEB x1) == (SEB x2) = show x1 == show x2

data FailureReason = ExpectedSatisfy { failureCallStack :: Maybe CallStack
                                                        , failureValue1 :: ShowEqBox} deriving Show

instance Exception FailureReason

shouldSatisfy :: (HasCallStack, MonadThrow m, Eq a, Show a) => a -> (a -> Bool) -> m ()
shouldSatisfy x sat
  | sat x = return ()
  | otherwise = throwIO (ExpectedSatisfy (Just callStack) (SEB x) )