module Iso.Prim where

import Prelude hiding (id,(.))

import Iso.Lib

import Control.Category
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class

mkIso :: Monad m =>  (a -> b) -> (b -> a) -> Iso m a b
mkIso f g = Iso (pure . f) (pure . g)

inverse :: Iso m a b -> Iso m b a
inverse (Iso f g) = Iso g f

infix 4 &&>, <&&

(<&&) :: MonadPlus m => Iso m a b -> Iso m a () -> Iso m a b
iso1 <&& iso2 = iunit . (iso1 &&& iso2)

(&&>) :: MonadPlus m => Iso m a () -> Iso m a b -> Iso m a b
iso1 &&> iso2 = iunit . commute . (iso1 &&& iso2)

ignoreAny :: Monad m => a -> Iso m a ()
ignoreAny a = mkIso f g where
    f _  = ()
    g () = a

-- | Nested products associate.
associate :: Monad m => Iso m (alpha, (beta, gamma)) ((alpha, beta), gamma)
associate = mkIso f g where
  f (a, (b, c)) = ((a, b), c)
  g ((a, b), c) = (a, (b, c))

-- | Products commute.
commute :: Monad m => Iso m (alpha, beta) (beta, alpha)
commute = mkIso f f where
  f (a, b) = (b, a)

distribute :: Monad m => Iso m (Either a b,c) (Either (a,c) (b,c))
distribute = mkIso f g where
    f (Left  a ,b)  = Left (a,b)
    f (Right a ,b)  = Right (a,b)
    g (Left  (a,b)) = (Left a,b)
    g (Right (a,b)) = (Right a,b)

isoIterate :: MonadPlus t => Iso t alpha alpha -> Iso t alpha alpha
isoIterate step = (isoIterate step <+> id) . step <+> id

-- | `()` is the unit element for products.
unit :: Monad m => Iso m alpha (alpha, ())
unit = mkIso f g where
  f a = (a, ())
  g (a, ()) = a

iunit :: Monad m => Iso m (alpha, ()) alpha
iunit = inverse unit

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


