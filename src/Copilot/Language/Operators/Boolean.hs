--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Boolean
  ( (&&)
  , (||)
  , not
  , true
  , false
  ) where

import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import Copilot.Language.Operators.Constant (constant)
import Copilot.Language.Stream
import qualified Prelude as P

--------------------------------------------------------------------------------

true :: Stream Bool
true = constant True

false :: Stream Bool
false = constant False

infix 5 && 

(&&) :: Stream Bool -> Stream Bool -> Stream Bool
(Const False) && _ = false
_ && (Const False) = false
x && y             = Op2 Core.and x y

infix 5 ||

(||) :: Stream Bool -> Stream Bool -> Stream Bool
(Const True) || _ = true
_ || (Const True) = true
x || y            = Op2 Core.or x y

not :: Stream Bool -> Stream Bool
not (Const c) = (Const $ P.not c)
not x         = Op1 Core.not x

--------------------------------------------------------------------------------
