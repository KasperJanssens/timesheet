{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application.Environment where

import           Control.Concurrent.STM      (TVar, atomically, stateTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT, ask)
import           Data.Map                    (Map)
import           Data.Pool                   (Pool)
import           Data.UUID                   (UUID)
import           Database.Persist.Postgresql (SqlBackend)
import           Domain.Customer
import           Domain.Monthly              (Monthly, SpecificMonth)
import           Servant                     (Handler)

type AppM = ReaderT State Handler

type InMemStore a = Map UUID a

type MonthlyMap = Map SpecificMonth Monthly

type CustomerMap = InMemStore Customer

data State = State {monthlyMap :: TVar MonthlyMap,  poel :: Pool SqlBackend}

class InEnvironment env where
  fetchInMemStore :: State -> TVar (InMemStore env)
  runInEnvironment :: (forall domainType. InMemStore domainType -> (b, InMemStore domainType)) -> AppM b
  runInEnvironment stateF = do
    state <- ask
    let (inMemStore :: TVar (InMemStore env)) = fetchInMemStore state
    liftIO $ atomically $ stateTVar inMemStore stateF
