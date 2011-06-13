--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Prelude as P
import Copilot.Language
import Copilot.Language.Prelude
import Copilot.Library.Voting 

--------------------------------------------------------------------------------

-- > interpret 10 [] vote 
-- results in out of memory
vote :: Spec
vote = do 
  trigger "maj" true
    [ arg maj ]
  trigger "aMaj" true 
    [ arg $ aMajority ls maj ]
  where
  ls  = [a, b, c, d, e, f, g, h, i, j, k, l, m]
  maj = majority ls
  a = [0] ++ a + 1 :: Stream Word32
  b = [0] ++ b + 1
  c = [0] ++ c + 1
  d = [0] ++ d + 1
  e = [1] ++ e + 1
  f = [1] ++ f + 1
  g = [1] ++ g + 1
  h = [1] ++ h + 1
  i = [1] ++ i + 1
  j = [1] ++ j + 1
  k = [1] ++ k + 1
  l = [1] ++ l + 1
  m = [1] ++ m + 1

--------------------------------------------------------------------------------