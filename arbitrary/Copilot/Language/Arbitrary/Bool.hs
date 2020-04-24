{-# LANGUAGE FlexibleInstances #-}

module Copilot.Language.Arbitrary.Bool () where

import Prelude            hiding ((&&), (||), not, (++))

import Test.QuickCheck    hiding ((==>))

import Copilot.Language

instance Arbitrary (Stream Bool) where
  arbitrary = sized gen
    where
      gen 0 = elements [true, false]
      gen n = frequency [ (n,   append)
                        , (100, unop)
                        , (100, binop)
                        ]
        where
          rec = gen (n `Prelude.div` 2)

          append = do
            buff <- arbitrary
            s    <- rec
            return $ buff ++ s

          unop = do
            op <- elements [not]
            s  <- rec
            return $ op s

          binop = do
            op <- elements [(&&), (||), xor, (==>)]
            l  <- rec
            r  <- rec
            return $ l `op` r
