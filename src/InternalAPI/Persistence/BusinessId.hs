{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module InternalAPI.Persistence.BusinessId where

import           Data.Aeson.Types            (FromJSON, ToJSON)
import           Data.UUID                   (UUID, fromText, toText)
import           Database.Persist.Postgresql
import           Servant                     (FromHttpApiData)

--TODO this should maybe be placed somewhere else, it exposes a bit too much
newtype BusinessId a = BusinessId {uuid :: UUID} 
  deriving newtype (FromHttpApiData, Read)
  deriving (Eq, Show, FromJSON, ToJSON, Ord)

instance PersistFieldSql (BusinessId a) where
  sqlType _ = SqlString

instance PersistField (BusinessId a) where
  toPersistValue (BusinessId uuid) = PersistText $ toText uuid
  fromPersistValue (PersistText a) = maybe (Left "could not parse uuid") (Right . BusinessId) $ fromText a
  fromPersistValue _ = Left "Not a text column apparently"
