{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ExternalAPI.FrontEndTypes.DailyJson where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Time    (Day)
import           Domain.Daily
import           GHC.Generics (Generic)

toDaily :: DailyJson -> Daily
toDaily (DailyJson _ d w) = Daily d w

fromDaily :: Daily -> DailyJson
fromDaily (Daily d w) = DailyJson d d w

data DailyJson = DailyJson {id :: Day, day :: Day, workpacks :: [WorkPack]} deriving (FromJSON, ToJSON, Generic, Show, Eq)
