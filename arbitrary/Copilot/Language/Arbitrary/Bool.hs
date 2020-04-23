{-# LANGUAGE FlexibleInstances #-}

module Copilot.Language.Arbitrary.Bool () where

import Prelude            hiding ((&&), (||), not)

import Test.QuickCheck    hiding ((==>))

import Copilot.Language

instance Arbitrary (Stream Bool) where
  arbitrary = sized gen
    where
      gen 0 = elements [true, false]
      gen n = oneof [unop, binop]
        where
          rec = gen (n `Prelude.div` 2)

          unop = do
            op <- elements [not]
            s  <- rec
            return $ op s

          binop = do
            op <- elements [(&&), (||), xor, (==>)]
            l  <- rec
            r  <- rec
            return $ l `op` r
