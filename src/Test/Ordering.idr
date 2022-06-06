import Control.Eff

data Alice a = MkAlice
data Bob a = MkBob
data Charles a = MkCharles

alice : Alice ()
alice = MkAlice

alice_e : Eff [Alice] ()
alice_e = send alice


f_invalid : Eff [Alice, Bob] ()
f_invalid = do
   relax alice_e -- have to use relax if you only have one statement

f0 : Eff [Alice, Bob] ()
f0 = Prelude.do
   send alice
   -- ?help
   pure ()

f1 : Eff [Bob, Alice] ()
f1 = do
   relax f0 -- reorder

f2 : Eff [Alice, Bob, Charles] ()
f2 = relax f1


f2_auto : Eff [Alice, Bob, Charles] ()
f2_auto = do
   f0
   f1
