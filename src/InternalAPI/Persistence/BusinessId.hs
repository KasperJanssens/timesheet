{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module InternalAPI.Persistence.BusinessId where

import           Data.UUID                   (UUID, fromText, toText)
import           Database.Persist.Postgresql

newtype BusinessId = BusinessId {uuid :: UUID} deriving (Eq, Show)

instance PersistFieldSql BusinessId where
  sqlType _ = SqlString

instance PersistField BusinessId where
  toPersistValue (BusinessId uuid) = PersistText $ toText uuid
  fromPersistValue (PersistText a) = maybe (Left "could not parse uuid") (Right . BusinessId) $ fromText a
  fromPersistValue _ = Left "Not a text column apparently"
