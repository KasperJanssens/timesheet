{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.FrontEndTypes.DailyJson where

import           Data.Aeson           (FromJSON, ToJSON, eitherDecode')
import           Data.Bifunctor       (first)
import qualified Data.ByteString.Lazy as BSLazy
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as TextEnc
import           Data.Time            (Day)
import           Data.UUID            (UUID)
import           Domain.Daily
import           GHC.Generics         (Generic)
import           Servant.API          (FromHttpApiData (..))

toDaily :: DailyJson -> Daily
toDaily (DailyJson (DailyId _ cId cVat) d w) = Daily d w cId cVat

fromDaily :: Daily -> DailyJson
fromDaily (Daily d w cId cVat) = DailyJson (DailyId d cId cVat) d w

data DailyId = DailyId
  { dayId      :: Day,
    customerId :: UUID,
    companyVat :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)

instance FromHttpApiData DailyId where
  parseUrlPiece t = first Text.pack $ eitherDecode' $ BSLazy.fromStrict $ TextEnc.encodeUtf8 t

data DailyJson = DailyJson
  { dailyId   :: DailyId,
    day       :: Day,
    workpacks :: [WorkPack]
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)
