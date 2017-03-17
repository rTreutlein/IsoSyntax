{-# LANGUAGE Rank2Types #-}
module Iso.Util where

import Iso.Lib
import Iso.Syntax
import Iso.Prim

import Prelude hiding ((.),id,iterate)

import Control.Category
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad

isoIterate :: SynMonad t s => SynIso t alpha alpha -> SynIso t alpha alpha
isoIterate step = (isoIterate step <+> id) . step <+> id

isoFoldl :: (SynMonad t s, Eq b,Show b) => SynIso t (a,b) a -> SynIso t (a,[b]) a
isoFoldl i = rmsnd [] . isoIterate step
    where step = first i . associate . second (inverse cons)

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
