{-# LANGUAGE Rank2Types #-}
module Lib where

import Prelude hiding ((.),id)

import Control.Category
import Control.Monad
import Control.Applicative

instance (Monoid a) => Alternative (Either a) where
    empty = Left mempty
    Left _ <|> r = r
    r      <|> _ = r

instance (Monoid a) => MonadPlus (Either a) where
    mzero = Left mempty
    Left _ `mplus` r = r
    r      `mplus` _ = r

class Category a => Arrow a where
    first :: a b c -> a (b, d) (c, d)
    second :: a b c -> a (d, b) (d, c)
    (***) :: a b c -> a d e -> a (b,d) (c,e)
    (&&&) :: a b c -> a b d -> a b (c,d)

class Arrow a => ArrowPlus a where
    (<+>) :: a b c -> a b c -> a b c

class Arrow a => ArrowChoice a where
    left  :: a b c -> a (Either b d) (Either c d)
    right :: a b c -> a (Either d b) (Either d c)

data Iso m a b = Iso (a -> m b) (b -> m a)

instance (Monad m) => Category (Iso m) where
    id = Iso pure pure
    Iso bc cd . Iso ab ba = Iso (ab >=> bc) (cd >=> ba)

instance (MonadPlus m) => Arrow (Iso m) where
    first iso  = iso *** id
    second iso = id *** iso
    iso1 *** iso2 = Iso f g where
        f (a,b) = liftM2 (,) (apply iso1 a) (apply iso2 b)
        g (c,d) = liftM2 (,) (unapply iso1 c) (unapply iso2 d)
    iso1 &&& iso2 = Iso f g where
        f a = liftM2 (,) (apply iso1 a) (apply iso2 a)
        g (b,c) = (unapply iso1 b) `mplus` (unapply iso2 c)

instance (MonadPlus m) => ArrowPlus (Iso m) where
    iso1 <+> iso2 = Iso f g where
        f a = (apply iso1 a) `mplus` (apply iso2 a)
        g b = (unapply iso1 b) `mplus` (unapply iso2 b)

apply :: Iso m a b -> a -> m b
apply (Iso ab _) = ab

unapply :: Iso m a b -> b -> m a
unapply (Iso _ ba) = ba

inverse :: Iso m a b -> Iso m b a
inverse (Iso f g) = Iso g f

mkIso :: Monad m =>  (a -> b) -> (b -> a) -> Iso m a b
mkIso f g = Iso (pure . f) (pure . g)




