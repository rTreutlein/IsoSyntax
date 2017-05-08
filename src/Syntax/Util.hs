{-# LANGUAGE Rank2Types #-}
module Syntax.Util where

import Iso
import Syntax.Lib
import Syntax.Constructors

import Prelude hiding ((.),id)

import Control.Category
import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad

import Data.List (partition)

ignore :: (SynMonad t s, Eq a, Show a) => a -> SynIso t a ()
ignore a = Iso f g where
    f e | a == e    = pure ()
        | otherwise = lift . Left $ "Expected " ++ show a ++ "but got " ++ show e ++ "when ingoring."
    g () = pure a

insert :: (SynMonad t s, Eq a, Show a) => a -> SynIso t () a
insert a = inverse (ignore a)

addfst :: (SynMonad t s, Eq a, Show a) => a -> SynIso t b (a,b)
addfst a = commute . addsnd a

addsnd :: (SynMonad t s, Eq a, Show a) => a -> SynIso t b (b,a)
addsnd a = second (insert a) . unit

rmfst :: (SynMonad t s, Eq a, Show a) => a -> SynIso t (a,b) b
rmfst a = inverse (addfst a)

rmsnd :: (SynMonad t s, Eq a, Show a) => a -> SynIso t (b,a) b
rmsnd a = inverse (addsnd a)

-------------------------------------------------------------------------------
--List Utils
-------------------------------------------------------------------------------

isoFoldl :: (SynMonad t s, Eq b,Show b) => SynIso t (a,b) a -> SynIso t (a,[b]) a
isoFoldl i = rmsnd [] . isoIterate step
    where step = first i . associate . second (inverse cons)

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
