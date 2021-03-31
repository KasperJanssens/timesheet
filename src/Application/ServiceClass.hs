{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Application.ServiceClass where

import qualified Application.MapUtil as MapUtil
import           Data.Kind           (Type)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.UUID           (UUID)
import           Numeric.Natural     (Natural)

class ReadonlyService domainType where
  type FrontEndType domainType = fendt | fendt -> domainType
  type NewType domainType :: Type

  toFrontEndType :: domainType -> FrontEndType domainType
  fromFrontEndType :: FrontEndType domainType -> domainType
  fromNewType :: UUID -> NewType domainType -> domainType

  list :: Maybe Natural -> Maybe Natural -> Map UUID domainType -> ((Int, [FrontEndType domainType]), Map UUID domainType)
  list (Just startIndex) (Just endIndex) uuidMap =
    let as = MapUtil.takeSliceOfMapNat startIndex endIndex uuidMap
     in ((Map.size uuidMap, toFrontEndType <$> as), uuidMap)
  list _ _ uuidMap =
    let as = Map.elems uuidMap
     in ((length as, toFrontEndType <$> as), uuidMap)
  get :: UUID -> Map UUID domainType -> (Maybe (FrontEndType domainType), Map UUID domainType)
  get uuid uuidMap =
    let maybeA = Map.lookup uuid uuidMap
     in (toFrontEndType <$> maybeA, uuidMap)

class (ReadonlyService domainType) => StandardService domainType where
  insert :: UUID -> NewType domainType -> Map UUID domainType -> (FrontEndType domainType, Map UUID domainType)
  insert uuid b uuidMap =
    let a = fromNewType uuid b
     in (toFrontEndType a, Map.insert uuid a uuidMap)

  delete :: UUID -> Map UUID domainType -> (Maybe (FrontEndType domainType), Map UUID domainType)
  delete uuid uuidMap =
    let maybeA = Map.lookup uuid uuidMap
     in let updatedUuidMap = Map.delete uuid uuidMap
         in (toFrontEndType <$> maybeA, updatedUuidMap)

class (ReadonlyService domainType, StandardService domainType) => FullService domainType where
  update :: UUID -> FrontEndType domainType -> Map UUID domainType -> (FrontEndType domainType, Map UUID domainType)
  update uuid updatedA uuidMap =
    let updatedMap = Map.update (const . Just $ fromFrontEndType updatedA) uuid uuidMap
     in (updatedA, updatedMap)
