{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Lens (
    lens
    , over
    , set
    , view
    , (.~)
    , (^.)
    , (%~)
    , _1
    , _2
) where

import           Control.Applicative   ((<$>))
import           Data.Functor.Const    (Const (..), getConst)
import           Data.Functor.Identity (Identity (..), runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set l f o = runIdentity $ l (const $ Identity f) o

view :: Lens' s a -> s -> a              -- lookup value (getter)
view l object =  getConst $ l Const object


over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over l f object = runIdentity $ l (Identity . f) object


(.~) :: Lens s t a b -> b -> s -> t
(.~) l f o = runIdentity $ l (const $ Identity f) o

(^.) :: Lens s t a b -> s -> a
(^.) l object =  getConst $ l Const object

(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) l f object = runIdentity $ l (Identity . f) object

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\ b -> (b, x)) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (\ b -> (x, b)) <$> f a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get setter f s = setter s <$> f (get s)
