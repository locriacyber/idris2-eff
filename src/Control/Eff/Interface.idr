||| Interact with Standard library
module Control.Eff.Interface

import Control.MonadRec
import Control.Monad.Free
import Data.Union
import Control.Eff.Internal

%default total

export
Has IO fs => HasIO (Free (Union fs)) where
   liftIO = send

export
Has f fs => Cast (f t) (Eff fs t) where
  cast = send

export
Subset fs fs' => Cast (Eff fs a) (Eff fs' a) where
  cast = lift


||| Effect Handler using IO
||| @m This effect can be handled by using @IO from the standard lbirary
public export
interface ImplIO m where
  handleUsingIO : m a -> IO a

export
toHandler : Every ImplIO fs -> Handler IO fs
toHandler [] = []
toHandler (x :: xs) = (handleUsingIO @{x} :: toHandler xs)

export
Cast (Every ImplIO fs) (Handler IO fs) where
  cast = toHandler

export
runIO : {every: Every ImplIO fs} -> Eff fs a -> IO a
runIO {every} = runEff (toHandler every)
