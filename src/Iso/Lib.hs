{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
module Iso.Lib where

import Prelude hiding ((.),id)

import Data.Monoid
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Class

data Iso m a b = Iso { apply :: (a -> m b) , unapply :: (b -> m a) }

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
    iso1 +++ iso2 = Iso f g where
        f (Left b)   = Left  <$> apply iso1 b
        f (Right b') = Right <$> apply iso2 b'
        g (Left c)   = Left  <$> unapply iso1 c
        g (Right c') = Right <$> unapply iso2 c'
    iso1 ||| iso2 = Iso f g where
        f (Left x) = apply iso1 x
        f (Right x) = apply iso2 x
        g y = (Left <$> unapply iso1 y) `mplus` (Right <$> unapply iso2 y)
