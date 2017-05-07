module Iso.Prim where

import Prelude hiding (id,(.))
import Control.Category

import Iso.Lib
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

nil :: SynMonad t s => SynIso t () [a]
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

-------------------------------------------------------------------------------
--List Utils
-------------------------------------------------------------------------------

isoConcat :: Monad t =>  Iso t [[a]] [a]
isoConcat = mkIso f g where
    f = concat
    g = map (: [])

--For converting elements or tuples into lists
--Lists are needed as arguments to form Link Atoms
tolist1 :: (SynMonad t s,Show a) => SynIso t a [a]
tolist1 = Iso f g where
    f a   = pure [a]
    g [a] = pure a
    g a   = lift $ Left $ "Expecting List with exaclty two elements but got" ++ show a

tolist2 :: (SynMonad t s,Show a) => SynIso t (a,a) [a]
tolist2 = Iso f g where
    f (a,b) = pure [a,b]
    g [a,b] = pure (a,b)
    g a     = lift $ Left $ "Expecting List with exaclty two elements but got" ++ show a

partitionIso :: Monad t => (a -> Bool) -> Iso t [a] ([a],[a])
partitionIso p = mkIso f g where
    f = partition p
    g = uncurry (++)

isoDrop :: Monad t => Int -> Iso t [a] [a]
isoDrop i = mkIso (drop i) id

isoReverse :: Monad t => Iso t [a] [a]
isoReverse = mkIso reverse reverse

isoZip :: Monad t => Iso t ([a],[b]) [(a,b)]
isoZip = mkIso (uncurry zip) unzip

isoDistribute :: (SynMonad t s) => SynIso t (a,[b]) [(a,b)]
isoDistribute = isoZip . reorder
    where reorder = Iso f g
          f (a,b)   = pure (replicate (length b) a,b)
          g (a:_,b) = pure (a,b)
          g ([],_)  = lift $ Left "Got Empty list but need at least 1 elem."
