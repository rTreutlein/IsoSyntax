{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
module Syntax.Constructors where

import Prelude hiding (id,(.))

import Iso
import Syntax.Lib
import Syntax.TH

import Control.Category
import Control.Monad.Trans.Class
import Data.List (partition)

nil :: Monad t => Iso t () [a]
nil = mkIso f g where
    f () = []
    g [] = ()

cons :: SynMonad t s => SynIso t (a,[a]) [a]
cons = Iso f g where
    f (a,as) = pure (a:as)
    g [] = lift $ Left "Can't unconse empty list"
    g (a:as) = pure (a,as)

$(defineIsomorphisms ''Maybe)
$(defineIsomorphisms ''Either)
