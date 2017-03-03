module Syntax where

import Prelude hiding ((.),id)

import Lib

import Control.Category
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Class

import qualified Data.Map as M

type MyRWST = RWST () () () (Either String)

type SynIso a b = Iso MyRWST a b

type Syntax a = SynIso () a

mkSynIso :: (a -> b) -> (b -> a) -> SynIso a b
mkSynIso f g = Iso (pure . f) (pure . g)

tolist :: Monad m => Iso m (a,a) [a]
tolist = mkIso f g where
    f (a,b) = a:b:[]
    g [a,b] = (a,b)

toString :: (Monad m, Show a, Read a) => Iso m a String
toString = mkIso f g where
    f a = show a
    g s = read s

runMyRWST (RWST runRWST) = runRWST () ()

anytoken :: SynIso Char Char
anytoken = token $ const True

token :: (Char -> Bool) -> SynIso Char Char
token cb = Iso f g where
    f c = if cb c
            then pure c
            else lift $ Left "Parsing Wrong Token."
    g c = if cb c
             then pure c
             else lift $ Left "Printing Wrong Token."

nil :: SynIso [a] [b]
nil = Iso f g where
    f [] = pure []
    f _  = lift $ Left "Not empty"
    g [] = pure []
    g _  = lift $ Left "Not empty"


cons :: SynIso (a,[a]) [a]
cons = Iso f g where
    f (a,as) = pure $ a:as
    g (a:as) = pure $ (a,as)
    g _ = lift $ Left "Empty List"

wrapcons :: SynIso (a,[a]) (b,[b]) -> SynIso [a] [b]
wrapcons iso = cons . iso . inverse cons

many :: SynIso a b -> SynIso [a] [b]
many syn = (wrapcons (syn *** many syn)) <+> nil

some :: SynIso a b -> SynIso [a] [b]
some syn = (wrapcons (syn *** many syn))







