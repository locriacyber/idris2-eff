||| Interact with Standard library
module Control.Eff.Interface

import Control.Eff.Internal

%default total

export
Has IO fs => HasIO (Eff fs) where
   liftIO = send
