module Domain.Company where

import           Data.Text (Text)

data Company = Company {name :: Text, vatNumber :: Text, address :: Text, bankAccountNumber :: Text, lastInvoiceNumber :: Maybe Int} deriving Show
