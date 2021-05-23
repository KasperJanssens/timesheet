module Common.Helper where

import           Control.Applicative        (Alternative, empty)
import           Data.Either.Combinators    (rightToMaybe)
import           Data.Text                  (Text)
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy             (toStrict)
import           Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Read             as TextRead

intToText :: Int -> Text
intToText = toStrict . toLazyText . decimal

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded f a = if f a then pure a else empty

hasSize :: Foldable t => Int -> t a -> Bool
hasSize i foldable = length foldable == i

textToIntegral :: Integral a => Text -> Maybe a
textToIntegral t =
  let res = TextRead.decimal t
   in fst <$> rightToMaybe res
