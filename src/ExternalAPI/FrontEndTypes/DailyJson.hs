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
toDaily (DailyJson i d w cId cVat) = Daily i d w cId cVat

fromDaily :: Daily -> DailyJson
fromDaily (Daily i d w cId cVat) = DailyJson i d w cId cVat

data DailyId = DailyId
  { dayId      :: Day,
    customerId :: UUID,
    companyVat :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)

instance FromHttpApiData DailyId where
  parseUrlPiece t = first Text.pack $ eitherDecode' $ BSLazy.fromStrict $ TextEnc.encodeUtf8 t

data DailyJson = DailyJson
  { id          :: UUID,
    day         :: Day,
    workpacks   :: [WorkPack],
    cId         :: UUID,
    companyName :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)
