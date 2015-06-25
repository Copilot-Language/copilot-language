--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Copilot specifications.

{-# LANGUAGE Trustworthy #-}
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
  , StructData (..)
  , structarg
  , struct
  , structs
  , Property (..)
  , prop
  , properties
  ) where

import Control.Monad.Writer
import Data.List (foldl')

--import Copilot.Core (Typed, Struct)
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

properties :: [SpecItem] -> [Property]
properties =
  foldl' properties' []
  where
  properties' ls e =
    case e of
      PropertyItem p -> p : ls
      _              -> ls

structs :: [SpecItem] -> [StructData]
structs =
  foldl' structs' []
  where
  structs' ls e =
    case e of
      StructItem s -> s : ls
      _            -> ls

--------------------------------------------------------------------------------

data SpecItem
  = ObserverItem Observer
  | TriggerItem  Trigger
  | PropertyItem Property
  | StructItem StructData

--------------------------------------------------------------------------------

data Observer where
  Observer :: Typed a => String -> Stream a -> Observer

--------------------------------------------------------------------------------

data Property where
  Property :: String -> Stream Bool -> Property

--------------------------------------------------------------------------------

prop :: String -> Stream Bool -> Spec
prop name e = tell [PropertyItem $ Property name e]

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

structarg :: String -> Arg -> StructArg
structarg n a = StructArg { name_ = n, arg' = a }

--------------------------------------------------------------------------------

data StructData where
  StructData :: Core.Name -> [StructArg] -> StructData

--------------------------------------------------------------------------------

struct :: String -> [StructArg] -> Spec
struct name sargs = tell [StructItem $ StructData name sargs]

--------------------------------------------------------------------------------
