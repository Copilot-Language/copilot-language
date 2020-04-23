{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Copilot.Language.Arbitrary.Num () where

import Prelude

import Test.QuickCheck

import Copilot.Language

instance (Typed a, Integral a, Arbitrary a) => Arbitrary (Stream a) where
  arbitrary = sized gen
    where
      gen :: Int -> Gen (Stream a)
      gen 0 = constant <$> arbitrary
      gen n = oneof [binop]
        where
          rec = gen (n `Prelude.div` 2)

          binop = do
            op <- elements [(+), (-), (*), Copilot.Language.div]
            l  <- rec
            r  <- rec
            return $ l `op` r
