{-# LANGUAGE FlexibleInstances #-}

module Copilot.Language.Arbitrary.Num () where

import Prelude            hiding ((++))

import Test.QuickCheck

import Copilot.Language

instance Arbitrary (Stream Int8) where
  arbitrary = sized gen_int

gen_int :: (Integral a, Typed a, Arbitrary a) => Int -> Gen (Stream a)
gen_int 0 = constant <$> arbitrary
gen_int n = frequency [ (n,   append)
                      , (100, binop)
                      ]
  where
    rec = gen_int (n `Prelude.div` 2)

    append = do
      buff <- listOf1 arbitrary
      s    <- rec
      return $ buff ++ s

    binop = do
      op <- elements [(+), (-), (*)]
      l  <- rec
      r  <- rec
      return $ l `op` r
