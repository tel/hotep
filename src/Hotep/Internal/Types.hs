{-# LANGUAGE GADTs, DeriveFunctor #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Hotep.Internal.Types where

import Data.Typeable
import Control.Exception

data Pid m x

instance Eq (Pid m x) where
  (==) = undefined

instance Ord (Pid m x) where
  compare = undefined

type SomePid = Pid SomeMsg SomeExit

data Script m x r

instance Functor (Script m x) where
  fmap = undefined

instance Applicative (Script m x) where
  pure = undefined
  (<*>) = undefined

instance Monad (Script m x) where
  return = pure
  (>>=) = undefined

data Monitor

data Exit x
  = ExitReason x
  | ExitException SomeException
  deriving Functor

data SomeExit where
  SomeExit :: (Show x, Typeable x) => Exit x -> SomeExit

data SomeMsg where
  SomeMsg :: (Show m, Typeable m) => m -> SomeMsg
