{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewDaily where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Data.Time    (Day)
import           Data.UUID    (UUID)
import           Domain.Daily
import           GHC.Generics (Generic)

data NewDaily = NewDaily
  { day         :: Day,
    workpacks   :: [WorkPack],
    customerId  :: UUID,
    customerVat :: Text
  }
  deriving (FromJSON, ToJSON, Generic)
