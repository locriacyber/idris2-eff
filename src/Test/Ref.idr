import Control.Eff

main' : Eff [StateL "hello" Nat] Nat
main' = do
  foo <- read "hello"
  write "hello" foo
  read "hello"

main' : Eff [State Nat] Nat
main' = do
  foo <- read
  write foo
  read
