module Application.MapUtil where

import qualified Data.List       as List
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Numeric.Natural

takeSliceOfMapNat :: Natural -> Natural -> Map a b -> [b]
takeSliceOfMapNat start end m =
  snd <$> takeSliceOfMapKVNat start end m

takeSliceOfMapKVNat :: Natural -> Natural -> Map a b -> [(a, b)]
takeSliceOfMapKVNat start end m =
  List.take (fromEnum $ end - start) $ List.drop (fromEnum start) $ Map.toList m
