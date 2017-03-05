{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Iso.Syntax where

import Prelude hiding ((.),id)

import Iso.Lib

import Control.Category
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Class

import qualified Data.Map as M
import Data.List

type MyRWST r w s = RWST r w s (Either String)

type SynIso a b = forall r w s. Monoid w => Iso (MyRWST r w s) a b

type Syntax a = forall r w s. Monoid w => Iso (MyRWST r w (s,String)) () a

mkSynIso :: (a -> b) -> (b -> a) -> SynIso a b
mkSynIso f g = Iso (pure . f) (pure . g)

tolist :: SynIso (a,a) [a]
tolist = mkSynIso f g where
    f (a,b) = a:b:[]
    g [a,b] = (a,b)

toString :: (Show a, Read a) => SynIso a String
toString = mkIso f g where
    f a = show a
    g s = read s

just :: SynIso a (Maybe a)
just = Iso f g where
    f a = pure $ Just a
    g (Just a) = pure  a
    g Nothing  = lift $ Left "Expected Just but got Nothing."

nothing :: SynIso () (Maybe a)
nothing = Iso f g where
    f () = pure Nothing
    g Nothing  = pure ()
    g (Just _) = lift $ Left "Expected Nothing but got Just"

optional :: Syntax a -> Syntax (Maybe a)
optional syn = just . syn <+> nothing . text ""

runMyRWST (RWST runRWST) = runRWST () ()

anytoken :: Syntax Char
anytoken = token $ const True

token :: (Char -> Bool) -> Syntax Char
token cb = Iso f g where
    f () = do
        (state,s) <- get
        case s of
            (x:xs) -> if cb x
                         then put (state,xs) >> pure x
                         else lift $ Left "Wrong Token."
            []     -> lift $ Left "Nothing left to parse."
    g c = if cb c
             then lift $ Left "Printing Wrong Token."
             else do
                (state,xs) <- get
                put $ (state,c:xs)
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
some syn = cons . (syn &&& many syn)

-- | `text` parses\/prints a fixed text and consumes\/produces a unit value.
text :: String -> Syntax ()
text s = ignore s . string s

string :: String -> Syntax String
string s = Iso f g where
    f () = do
        (state,text) <- get
        if s `isPrefixOf` text
         then put (state,(drop (length s) text)) >> pure s
         else lift $ Left $ "Expected " ++ s ++ "but found " ++ text ++ " when parsing."
    g a = if a == s
             then modify (\(state,ss) -> (state,s ++ ss))
             else lift $ Left $ "Expected " ++ s ++ "but found " ++ a ++ " when printing."

-- | `skipSpace` marks a position where whitespace is allowed to
-- occur. It accepts arbitrary space while parsing, and produces
-- no space while printing.

skipSpace :: Syntax ()
skipSpace = ignore [] . many (text " ")

-- | `optSpace` marks a position where whitespace is desired to occur.
-- It accepts arbitrary space while parsing, and produces a
-- single space character while printing.

optSpace :: Syntax ()
optSpace = ignore [()] . many (text " ")

-- | `sepSpace` marks a position where whitespace is required to
-- occur. It requires one or more space characters while parsing,
-- and produces a single space character while printing.

sepSpace :: Syntax ()
sepSpace = text " " . skipSpace
