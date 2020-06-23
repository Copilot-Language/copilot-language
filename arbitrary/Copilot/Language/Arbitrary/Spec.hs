{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Copilot.Language.Arbitrary.Spec () where

import Test.QuickCheck

import Copilot.Language            hiding (prettyPrint, div)
import Copilot.Language.Reify
import Copilot.Language.Stream            (Arg (..))
import Copilot.Core.PrettyPrint           (prettyPrint)

import Copilot.Language.Arbitrary.Bool    ()
import Copilot.Language.Arbitrary.Num     ()

import System.IO.Unsafe                   (unsafePerformIO)


instance {-# OVERLAPPING #-} Show Spec where
  show spec = (prettyPrint . unsafePerformIO . reify) spec

instance Arbitrary Spec where
  arbitrary = sized gen_spec
    where
      gen_spec n = do
        frequency [ (1, gen_trig)
                  , (n, rec n)
                  ]
      gen_trig = trigger <$> gen_cident <*> arbitrary <*> arbitrary

      rec n = do
        t  <- gen_trig
        ts <- gen_spec (n `div` 3)
        return $ t >> ts

instance Arbitrary Arg where
  arbitrary = oneof [ Arg <$> (arbitrary :: Gen (Stream Int8))
                    , Arg <$> (arbitrary :: Gen (Stream Int16))
                    , Arg <$> (arbitrary :: Gen (Stream Int32))
                    , Arg <$> (arbitrary :: Gen (Stream Int64))
                    , Arg <$> (arbitrary :: Gen (Stream Word8))
                    , Arg <$> (arbitrary :: Gen (Stream Word16))
                    , Arg <$> (arbitrary :: Gen (Stream Word32))
                    ]

-- | Generate a valid C identifier name.
gen_cident :: Gen String
gen_cident = (:) <$> alpha <*> (listOf alphanum)
  where
    alpha    = elements $ ['a'..'z'] Prelude.++ ['A'..'Z']
    num      = elements ['0'..'9']
    alphanum = oneof [alpha, num]
