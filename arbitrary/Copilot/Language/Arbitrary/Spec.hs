{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Copilot.Language.Arbitrary.Spec () where

import Test.QuickCheck

import Copilot.Language
import Copilot.Language.Stream            (Arg (..))

import Copilot.Language.Arbitrary.Bool    ()
import Copilot.Language.Arbitrary.Num     ()

instance Arbitrary Spec where
  arbitrary = trigger <$> gen_cident <*> arbitrary <*> arbitrary

instance Arbitrary Arg where
  arbitrary = Arg <$> (arbitrary :: Gen (Stream Int8))

-- | Generate a valid C identifier name.
gen_cident :: Gen String
gen_cident = (:) <$> alpha <*> (listOf alphanum)
  where
    alpha    = elements $ ['a'..'z'] Prelude.++ ['A'..'Z']
    num      = elements ['0'..'9']
    alphanum = oneof [alpha, num]
