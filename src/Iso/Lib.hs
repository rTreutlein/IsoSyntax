{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
module Iso.Lib where

import Prelude hiding ((.),id)

import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Class

infixr 5 <+>
infixr 3 ***
infixr 3 &&&
infixr 2 |||
infixr 2 +++

{-newtype MEither a b = MEither {runMEither :: Either a b}
    deriving (Functor,Applicative,Monad)

instance (Monoid a) => Alternative (MEither a) where
    empty = MEither $ Left mempty
    (MEither (Left _)) <|> r = r
    r                  <|> _ = r

instance (Monoid a) => MonadPlus (MEither a) where
    mzero = empty
    mplus = (<|>)
-}
class Category a => Arrow a where
    first :: a b c -> a (b, d) (c, d)
    second :: a b c -> a (d, b) (d, c)
    (***) :: a b c -> a d e -> a (b,d) (c,e)
    (&&&) :: a b c -> a b d -> a b (c,d)

class Arrow a => ArrowZero a where
    zeroArrow :: a b c

class ArrowZero a => ArrowPlus a where
    (<+>) :: a b c -> a b c -> a b c

class Arrow a => ArrowChoice a where
    --left  :: a b c -> a (Either b d) (Either c d)
    --right :: a b c -> a (Either d b) (Either d c)
    (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
    (|||) :: a b d -> a c d -> a (Either b c) d

data Iso m a b = Iso (a -> m b) (b -> m a)

instance (Monad m) => Category (Iso m) where
    id = Iso pure pure
    Iso bc cd . Iso ab ba = Iso (ab >=> bc) (cd >=> ba)

instance (MonadPlus m) => Arrow (Iso m) where
    first iso  = iso *** id
    second iso = id *** iso
    iso1 *** iso2 = Iso f g where
        f (a,b) = (,) <$> apply iso1 a   <*> apply iso2 b
        g (c,d) = (,) <$> unapply iso1 c <*> unapply iso2 d
    iso1 &&& iso2 = Iso f g where
        f a = liftM2 (,) (apply iso1 a) (apply iso2 a)
        g (b,c) = unapply iso1 b `mplus` unapply iso2 c

instance (MonadPlus m) => ArrowZero (Iso m) where
    zeroArrow = Iso f g where
        f _ = mzero
        g _ = mzero

instance (MonadPlus m) => ArrowPlus (Iso m) where
    iso1 <+> iso2 = Iso f g where
        f a = apply iso1 a `mplus` apply iso2 a
        g b = unapply iso1 b `mplus` unapply iso2 b

instance (MonadPlus m) => ArrowChoice (Iso m) where
    --left  = (+++ id)
    --right = (id +++)
    iso1 +++ iso2 = Iso f g where
        f (Left b)   = Left  <$> apply iso1 b
        f (Right b') = Right <$> apply iso2 b'
        g (Left c)   = Left  <$> unapply iso1 c
        g (Right c') = Right <$> unapply iso2 c'
    iso1 ||| iso2 = iso1 +++ iso2 >>> untag
        where untag = Iso f g
              f (Left x) = pure x
              f (Right x) = pure x
              g = pure . Left

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

apply :: Iso m a b -> a -> m b
apply (Iso ab _) = ab

unapply :: Iso m a b -> b -> m a
unapply (Iso _ ba) = ba

inverse :: Iso m a b -> Iso m b a
inverse (Iso f g) = Iso g f

mkIso :: Monad m =>  (a -> b) -> (b -> a) -> Iso m a b
mkIso f g = Iso (pure . f) (pure . g)

ignoreAny :: Monad m => a -> Iso m a ()
ignoreAny a = mkIso f g where
    f _  = ()
    g () = a

insertAny :: Monad m => a -> Iso m () a
insertAny a = inverse (ignoreAny a)

addfstAny :: MonadPlus m => a -> Iso m b (a,b)
addfstAny a = commute . addsndAny a

addsndAny :: MonadPlus m => a -> Iso m b (b,a)
addsndAny a = second (insertAny a) . unit

rmfstAny :: MonadPlus m => a -> Iso m (a,b) b
rmfstAny a = inverse (addfstAny a)

rmsndAny :: MonadPlus m => a -> Iso m (b,a) b
rmsndAny a = inverse (addsndAny a)

-- | Nested products associate.
associate :: Monad m => Iso m (alpha, (beta, gamma)) ((alpha, beta), gamma)
associate = mkIso f g where
  f (a, (b, c)) = ((a, b), c)
  g ((a, b), c) = (a, (b, c))

-- | Products commute.
commute :: Monad m => Iso m (alpha, beta) (beta, alpha)
commute = mkIso f f where
  f (a, b) = (b, a)

-- | `()` is the unit element for products.
unit :: Monad m => Iso m alpha (alpha, ())
unit = mkIso f g where
  f a = (a, ())
  g (a, ()) = a

iunit :: Monad m => Iso m (alpha, ()) alpha
iunit = inverse unit

(<&&) :: MonadPlus m => Iso m a b -> Iso m a () -> Iso m a b
iso1 <&& iso2 = iunit . (iso1 &&& iso2)

(&&>) :: MonadPlus m => Iso m a () -> Iso m a b -> Iso m a b
iso1 &&> iso2 = iunit . commute . (iso1 &&& iso2)
