{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.FrontEndTypes.DailyJson where

import           Data.Aeson                         (FromJSON, ToJSON,
                                                     eitherDecode')
import           Data.Bifunctor                     (first)
import qualified Data.ByteString.Lazy               as BSLazy
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as TextEnc
import           Data.Time                          (Day)
import           Data.UUID                          (UUID)
import           Domain.Customer
import           Domain.Daily
import           GHC.Generics                       (Generic)
import           InternalAPI.Persistence.BusinessId
import           Servant.API                        (FromHttpApiData (..))

toDaily :: DailyJson -> Daily
toDaily (DailyJson i d w cId cVat alreadyInvoice) = Daily i d w cId cVat alreadyInvoice

fromDaily :: Daily -> DailyJson
fromDaily (Daily i d w cId cVat alreadyInvoice) = DailyJson i d w cId cVat alreadyInvoice

data DailyJson = DailyJson
  { id             :: BusinessId Daily,
    day            :: Day,
    workpacks      :: [WorkPack],
    customerId     :: BusinessId Customer,
    companyVat     :: Text,
    alreadyInvoice :: Bool
  }
  deriving (FromJSON, ToJSON, Generic, Show, Eq)
