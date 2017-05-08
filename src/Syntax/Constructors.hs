module Syntax.Constructors where

import Prelude hiding (id,(.))

import Iso
import Syntax.Lib

import Control.Category
import Control.Monad.Trans.Class
import Data.List (partition)

just :: SynMonad t s => SynIso t a (Maybe a)
just = Iso f g where
    f a = pure $ Just a
    g (Just a) = pure  a
    g Nothing  = lift $ Left "Expected Just but got Nothing."

nothing :: SynMonad t s => SynIso t () (Maybe a)
nothing = Iso f g where
    f () = pure Nothing
    g Nothing  = pure ()
    g (Just _) = lift $ Left "Expected Nothing but got Just"

nil :: Monad t => Iso t () [a]
nil = mkIso f g where
    f () = []
    g [] = ()

cons :: SynMonad t s => SynIso t (a,[a]) [a]
cons = Iso f g where
    f (a,as) = pure (a:as)
    g [] = lift $ Left "Can't unconse empty list"
    g (a:as) = pure (a,as)


left :: SynMonad t s => SynIso t a (Either a b)
left = Iso f g where
    f a = pure $ Left a
    g (Left a) = pure a
    g (Right _) = lift $ Left "Was Expecting Left but got Right"

right :: SynMonad t s => SynIso t b (Either a b)
right = Iso f g where
    f a = pure $ Right a
    g (Right a) = pure a
    g (Left _) = lift $ Left "Was Expecting Right but got Left"


