{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Iso.Syntax where

import Prelude hiding ((.),id)

import Iso.Lib
import Iso.Prim

import Control.Category
import Control.Monad.State.Class
import Control.Monad.Trans.Class

import qualified Control.Arrow as A

import qualified Data.Map as M
import Data.List

syntax :: SynMonad t s =>
        (String -> Either String (a,String))
       -> (a -> Either String String)
       -> Syntax t a
syntax sf sg = Iso f g where
    f () = do
        text <- gets getText
        case sf text of
            Right (a,nt) -> modify (setText nt) >> pure a
            Left l       -> lift $ Left l
    g a = case sg a of
            Right out -> modify (addText out)
            Left l    -> lift $ Left l

anytoken :: SynMonad t s => Syntax t Char
anytoken = token $ const True

token :: SynMonad t s => (Char -> Bool) -> Syntax t Char
token cb = syntax f g where
    f s = case s of
            (c:cs) -> if cb c
                     then Right (c,cs)
                        else Left "Wrong token"
            []     -> Left "Nothing left to parse."
    g c = if cb c
             then Right [c]
             else Left "Printing wrong token."

string :: SynMonad t s => String -> Syntax t String
string s = syntax f g where
    f t = let (text,res) = splitAt (length s) t
          in if text == s
                then Right (text,res)
                    else Left ("Expected " ++ s ++ " but found " ++  text)
    g t = if t == s
             then Right s
             else Left ("Expected " ++ s ++ " but found " ++ t)

withText :: SynMonad t s => Syntax t a -> Syntax t String
withText syn = Iso f g where
    f () = do
        text <- gets getText
        len1 <- pure $ length text
        apply syn ()
        len2 <- length <$> gets getText
        pure (take (len1-len2) text)
    g s = modify (addText s)

reparse :: SynMonad t s => Syntax t a -> SynIso t String a
reparse syn = Iso f g where
    f s = do
        modify (addText s)
        apply syn ()
    g a = do
        len1 <- length <$> gets getText
        unapply syn a
        text <- gets getText
        len2 <- pure $ length text
        pure (take (len2-len1) text)


--Will run the parser without consuming input
lookahead :: SynMonad t s => Syntax t a -> Syntax t a
lookahead syn = Iso f g where
    f () = do
        text <- gets getText
        a <- apply syn ()
        modify (setText text)
        pure a
    g _ = pure ()

ptp :: SynMonad t s => Syntax t a -> SynIso t String String -> Syntax t b -> Syntax t b
ptp syn1 iso syn2 = withText syn1 >>> iso >>> reparse syn2
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

optional :: SynMonad t s => Syntax t a -> Syntax t (Maybe a)
optional syn = just . syn <+> nothing . text ""

many :: SynMonad t s => Syntax t a -> Syntax t [a]
many syn = (cons . (syn &&& many syn)) <+> nil

some :: SynMonad t s => Syntax t a -> Syntax t [a]
some syn = cons . (syn &&& many syn)

-- | `text` parses\/prints a fixed text and consumes\/produces a unit value.
text :: SynMonad t s => String -> Syntax t ()
text s = ignoreAny s . string s

-- | `skipSpace` marks a position where whitespace is allowed to
-- occur. It accepts arbitrary space while parsing, and produces
-- no space while printing.

skipSpace :: SynMonad t s => Syntax t ()
skipSpace = ignoreAny [] . many (text " ")

-- | `optSpace` marks a position where whitespace is desired to occur.
-- It accepts arbitrary space while parsing, and produces a
-- single space character while printing.

optSpace :: SynMonad t s => Syntax t ()
optSpace = ignoreAny [()] . many (text " ")

-- | `sepSpace` marks a position where whitespace is required to
-- occur. It requires one or more space characters while parsing,
-- and produces a single space character while printing.

sepSpace :: SynMonad t s => Syntax t ()
sepSpace = text " " <&& skipSpace
