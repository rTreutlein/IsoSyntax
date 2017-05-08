{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Syntax.Lib where

import Prelude hiding ((.),id)

import Iso.Lib
import Iso.Prim

import Control.Category
import Control.Arrow
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Trans.Class

import qualified Data.Map as M
import Data.List

class SyntaxState a where
    getText :: a -> String
    addText :: String -> a -> a
    setText :: String -> a -> a

type ME = Either String
type SynMonad t s = (MonadTrans t
                    ,MonadPlus (t ME)
                    ,SyntaxState s
                    ,MonadState s (t ME)
                    )

type SynIso t a b = Iso (t ME) a b
type Syntax t a = Iso (t ME) () a


