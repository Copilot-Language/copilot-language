--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Copilot specifications.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Copilot.Language.Spec
  ( Spec
  , runSpec
  , SpecItem
  , Observer (..)
  , observer
  , observers
  , Trigger (..)
  , triggers
  , trigger
  , arg
  ) where

import Control.Monad.Writer
import Data.List (foldl')

import Copilot.Core (Typed)
import qualified Copilot.Core as Core
import Copilot.Language.Stream

--------------------------------------------------------------------------------

type Spec = Writer [SpecItem] ()

--------------------------------------------------------------------------------

runSpec :: Spec -> [SpecItem]
runSpec = execWriter 

--------------------------------------------------------------------------------

observers :: [SpecItem] -> [Observer]
observers = 
  foldl' lets' []
  where
  lets' ls e =
    case e of 
      ObserverItem l -> l : ls
      _              -> ls

triggers :: [SpecItem] -> [Trigger]
triggers = 
  foldl' triggers' []
  where
  triggers' ls e =
    case e of 
      TriggerItem t -> t : ls
      _             -> ls

--------------------------------------------------------------------------------

data SpecItem
  = ObserverItem Observer
  | TriggerItem  Trigger

--------------------------------------------------------------------------------

data Observer where
  Observer :: Typed a => String -> Stream a -> Observer

--------------------------------------------------------------------------------

observer :: Typed a => String -> Stream a -> Spec
observer name e = tell [ObserverItem $ Observer name e]

--------------------------------------------------------------------------------

data Trigger where
  Trigger :: Core.Name -> Stream Bool -> [Arg] -> Trigger

--------------------------------------------------------------------------------

trigger :: String -> Stream Bool -> [Arg] -> Spec
trigger name e args = tell [TriggerItem $ Trigger name e args]

--------------------------------------------------------------------------------

arg :: Typed a => Stream a -> Arg
arg = Arg

--------------------------------------------------------------------------------
