{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.WorkTypeJson where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data WorkTypeJson = WorkTypeJson
  { id   :: Text,
    name :: Text
  }
  deriving (FromJSON, ToJSON, Generic, Show)
