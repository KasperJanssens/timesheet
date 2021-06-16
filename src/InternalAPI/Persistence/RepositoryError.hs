module InternalAPI.Persistence.RepositoryError where

import           Control.Exception (Exception)
import           Data.Text         (Text)

data RepositoryError = RepositoryError Text deriving Show

instance Exception RepositoryError
