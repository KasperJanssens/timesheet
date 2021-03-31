{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.NewTypes.NewDaily where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Time    (Day)
import           Domain.Daily
import           GHC.Generics (Generic)

data NewDaily = NewDaily {day ::Day, workpacks :: [WorkPack]} deriving (FromJSON, ToJSON, Generic)
