data Context : Type where
   -- ...

main' : Eff [Console, State Context] ()

main : IO ()
main = runIO main'
