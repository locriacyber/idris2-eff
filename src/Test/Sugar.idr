import Control.Eff
import Control.Eff.Sugar

data Alice a = MkAlice
data Bob a = MkBob
data Charles a = MkCharles

alice : Alice ()
alice = MkAlice

-- this below is still broken
f0 : Eff [Alice, Bob] ()
f0 = do
   send alice -- Error: While processing right hand side of f0. Can't find an implementation for Subset ?fs0 [Alice, Bob].
   send MkBob


f1 : Eff [Bob, Alice] ()
f1 = do
   lift f0 -- need `lift` if only one statement

fio : Eff [IO, Bob, Alice] ()
fio = do
   line <- getLine -- HasIO is broken as well
   putStrLn line
   f1
