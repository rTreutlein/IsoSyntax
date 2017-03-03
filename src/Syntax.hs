module Syntax where

import Prelude hiding ((.),id)

import Lib

import Control.Category
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Class

import qualified Data.Map as M

type WordList = (M.Map String [String],[String],[(String,String)],Int)

type MyRWST = RWST WordList () String (Either String)

type SynIso a b = Iso MyRWST a b

type Syntax a = SynIso () a

mkSynIso :: (a -> b) -> (b -> a) -> SynIso a b
mkSynIso f g = Iso (pure . f) (pure . g)

anytoken :: Syntax Char
anytoken = token $ const True

token :: (Char -> Bool) -> Syntax Char
token cb = Iso f g where
    f () = do
        s <- get
        case s of
            (x:xs) -> if cb x
                         then put xs >> pure x
                         else lift $ Left "Wrong Token."
            []     -> lift $ Left "Nothing left to parse."
    g c = do
        case cb c of
          False -> lift $ Left "Printing Wrong Token."
          True -> do
              xs <- get
              put $ c:xs
              pure ()

nil :: Syntax [a]
nil = mkSynIso f g where
    f () = []
    g [] = ()

cons :: SynIso (a,[a]) [a]
cons = mkSynIso f g where
    f (a,as) = a:as
    g (a:as) = (a,as)

many :: Syntax a -> Syntax [a]
many syn = (cons . (syn &&& many syn)) <+> nil

some :: Syntax a -> Syntax [a]
some syn = cons . (syn &&& (some syn <+> nil))

