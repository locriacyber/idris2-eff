module Control.Eff.Legacy

import Control.Eff.Internal

||| handle effect with state
export
handleRelayS :  (prf : Has f fs)
             => s
             -> (s -> a -> Eff (fs - f) b)
             -> (forall v . s -> f v -> (s -> v -> Eff (fs - f) b) -> Eff (fs - f) b)
             -> Eff fs a
             -> Eff (fs - f) b
handleRelayS vs fval fcont fr = case toView fr of
  Pure val => fval vs val
  Bind x g => case decomp {prf} x of
    Left y  => assert_total $ lift y >>= handleRelayS vs fval fcont . g
    Right y => assert_total $ fcont vs y (\vs2 => handleRelayS vs2 fval fcont . g)
