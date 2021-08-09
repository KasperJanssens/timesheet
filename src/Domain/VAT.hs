{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Domain.VAT where

import           Control.Applicative
import           Data.Aeson.Types            (FromJSON (..), ToJSON (..),
                                              Value (..), parseFail,
                                              prependFailure, typeMismatch)
import           Data.Attoparsec.Text
import           Data.Bifunctor              (first)
import           Data.Char                   (digitToInt, isSpace)
import           Data.String.Interpolate     (i)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Database.Persist.Postgresql (PersistField (..),
                                              PersistFieldSql (..),
                                              PersistValue (..), SqlType (..))

import qualified Text.Printf                 as Text

--https://www.btw-nummer-controle.nl/Userfiles/images/Format%20btw-nummers%20EU(4).pdf
--https://help.afas.nl/help/NL/SE/Fin_Config_VatIct_NrChck.htm

data VAT = BE Int Int | NL Int Int Int deriving (Eq, Ord)

maybeCreateBelgianVAT:: Int -> Int -> Maybe VAT
maybeCreateBelgianVAT nbr mod97 = if isValidBelgianVATNumber nbr mod97 then
  return (BE nbr mod97)
  else
    Nothing

instance PersistField VAT where
  toPersistValue vat = PersistText $ prettyPrint vat
  fromPersistValue (PersistText t) = first Text.pack $ Domain.VAT.parse t
  fromPersistValue _               = Left "No text in column, cannot work"

instance PersistFieldSql VAT where
  sqlType _ = SqlString

stripSpace :: Text -> Text
stripSpace = Text.filter (not . isSpace )

instance FromJSON VAT where
  parseJSON (String t) =
    let eitherVAT =  Domain.VAT.parse $ stripSpace t in
      case eitherVAT of
        Left s  -> parseFail s
        Right v -> return v
  parseJSON invalid    =
      prependFailure "parsing VAT failed, "
          (typeMismatch "String" invalid)


instance ToJSON VAT where
  toJSON vat = String $ prettyPrint vat

instance Show VAT where
  show = Text.unpack . prettyPrint

prettyPrint :: VAT -> Text
prettyPrint (BE real control)      = Text.concat ["BE0", Text.pack $ Text.printf "%06d" real, Text.pack $ Text.printf "%02d" control]
prettyPrint (NL real eleven mod97) = Text.concat ["NL", Text.pack $ Text.printf "%08d" real, Text.pack $ Text.printf "%01d" eleven, "B", Text.pack $ Text.printf "%02d" mod97]

isZero :: Char -> Bool
isZero '0' = True
isZero _   = False

isB :: Char -> Bool
isB 'B' = True
isB _   = False

vatParser :: Parser VAT
vatParser =
  (asciiCI "BE" >> parseBelgianVATNumber)
    <|> (asciiCI "NL" >> parseDutchVATNumber)

parse :: Text -> Either String VAT
parse = Data.Attoparsec.Text.parseOnly vatParser

isValidBelgianVATNumber :: Int -> Int -> Bool
isValidBelgianVATNumber realNbr controlDigits =
  97 - (realNbr `mod` 97) == controlDigits

elevenTest :: Int -> Int -> Bool
elevenTest realVatNumber elevenTestNumber =
  ( ((realVatNumber `div` 10000000) * 9)
      + ((realVatNumber `div` 1000000 `mod` 10) * 8)
      + ((realVatNumber `div` 100000 `mod` 10) * 7)
      + ((realVatNumber `div` 10000 `mod` 10) * 6)
      + ((realVatNumber `div` 1000 `mod` 10) * 5)
      + ((realVatNumber `div` 100 `mod` 10) * 4)
      + ((realVatNumber `div` 10 `mod` 10) * 3)
      + ((realVatNumber `mod` 10) * 2)
      - elevenTestNumber
  )
    `mod` 11
    == 0

isValidDutchVATNumber :: Int -> Int -> Int -> Bool
isValidDutchVATNumber realVatNumber elevenTestNumber mod97TestNumber =
  elevenTest realVatNumber elevenTestNumber || realVatNumber `mod` 97 == mod97TestNumber

parseBelgianVATNumber :: Parser VAT
parseBelgianVATNumber = do
  _ <- satisfy isZero
  realVatNumber <- read <$> count 7 digit
  controlDigits <- read <$> count 2 digit
  if isValidBelgianVATNumber realVatNumber controlDigits then return (BE realVatNumber controlDigits) else fail "Belgian VAT Number incorrect control digits "

parseDutchVATNumber :: Parser VAT
parseDutchVATNumber = do
  realVatNumber <- read <$> count 8 digit
  elevenTestNumber <- digitToInt <$> digit
  _ <- satisfy isB
  mod97TestNumber <- read <$> count 2 digit
  if isValidDutchVATNumber realVatNumber elevenTestNumber mod97TestNumber then return (NL realVatNumber elevenTestNumber mod97TestNumber) else fail "Dutch VAT number eleven test or mod 97 test failed"
