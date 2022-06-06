||| Interact with Standard library
module Control.Eff.Interface

import Control.MonadRec
import Control.Monad.Free
import Data.Union
import Control.Eff.Internal

export
Has IO fs => HasIO (Free (Union fs)) where
   liftIO = send


export
(>>=) : Subset fs0 fs2 => Subset fs1 fs2 => Eff fs0 a -> (a -> Eff fs1 b) -> Eff fs2 b
(>>=) val cont = Prelude.do
   a <- relax val
   relax (cont a)

export
(>>) :  Subset fs0 fs2 => Subset fs1 fs2 => Eff fs0 () -> Eff fs1 a -> Eff fs2 a
(>>) @{s0} @{s1} v0 v1 = Prelude.(>>) (relax @{s0} v0) (relax @{s1} v1)

||| Inject value with null effect
export
pure : a -> Eff [] a
pure x = fromView (Pure x)
