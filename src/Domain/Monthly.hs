{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Monthly where

import           Data.Aeson     (FromJSON, ToJSON)
import           Data.Bifunctor (first)
import           Data.Text      (Text)
import qualified Data.Text      as Text
import qualified Data.Text.Read as TextRead
import           Data.UUID      (UUID)
import           Debug.Trace
import           Domain.Daily   (Daily)
import           GHC.Generics   (Generic)
import           Servant.API    (FromHttpApiData (..))
import           Text.Builder   (decimal, run)

type Year = Integer

type MonthOfYear = Int

data SpecificMonth = SpecificMonth
  { y :: Year,
    m :: MonthOfYear
  }
  deriving (FromJSON, ToJSON, Generic, Ord, Eq, Show)

instance FromHttpApiData SpecificMonth where
  parseUrlPiece t =
    let split = trace ("text to parse is " ++ show t) $ Text.split (== ',') t
     in do
          (yAsText, mAsText) <-
            if length split == 2
              then Right (head split, split !! 1)
              else Left "not exactly two pieces"
          (y, _) <- first Text.pack $ TextRead.decimal yAsText
          (m, _) <- first Text.pack $ TextRead.decimal mAsText
          return $ SpecificMonth y m

toTextMonth :: MonthOfYear -> Text
toTextMonth 0  = "Januari"
toTextMonth 1  = "Februari"
toTextMonth 2  = "Maart"
toTextMonth 3  = "April"
toTextMonth 4  = "Mei"
toTextMonth 5  = "Juni"
toTextMonth 6  = "Juli"
toTextMonth 7  = "Augustus"
toTextMonth 8  = "September"
toTextMonth 9  = "Oktober"
toTextMonth 10 = "November"
toTextMonth 11 = "December"

toText :: Year -> MonthOfYear -> Text
toText y m = Text.intercalate "," [toTextMonth m, run $ decimal y]

create :: Year -> MonthOfYear -> Daily -> Monthly
create year month daily = Monthly (SpecificMonth year month) (toText year month) [daily]

data Monthly = Monthly {id :: SpecificMonth, month :: Text, dailies :: [Daily]} deriving (FromJSON, ToJSON, Generic, Show, Eq)
