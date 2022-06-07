||| Syntax sugar for `do` notation.
module Control.Eff.Sugar

import Control.Eff.Internal
import Data.Subset

%default total

export
(>>=) : Subset fs0 fs2 => Subset fs1 fs2 => Eff fs0 a -> (a -> Eff fs1 b) -> Eff fs2 b
(>>=) val cont = Prelude.do
   a <- lift val
   lift (cont a)

export
(>>) : Subset fs0 fs2 => Subset fs1 fs2 => Eff fs0 () -> Eff fs1 a -> Eff fs2 a
(>>) @{s0} @{s1} v0 v1 = Prelude.do
   lift @{s0} v0
   lift @{s1} v1

||| Inject value with null effect
export
pure : a -> Eff [] a
pure x = fromView (Pure x)

export
join : Subset fs0 fs2 => Subset fs1 fs2 => Eff fs0 (Eff fs1 a) -> Eff fs2 a
join @{s0} @{s1} v = Prelude.do
   v' <- lift @{s0} v
   lift @{s1} v'
