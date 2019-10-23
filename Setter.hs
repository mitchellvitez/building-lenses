{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Setter where

import Data.Coerce (coerce, Coercible)

infixr 9 #.

(#.) :: Coercible c b => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b

class Functor g => Distributive g where
  distribute :: Functor f => f (g a) -> g (f a)
  distribute = collect id

  collect :: Functor f => (a -> g b) -> f a -> g (f b)
  collect f = distribute . fmap f

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity
  where fmap = coerce

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

instance Foldable Identity where
  foldMap = coerce

instance Traversable Identity where
  traverse f = sequenceA . fmap f

instance Distributive Identity where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall a b f . Functor f => (a -> Identity b) -> f a -> Identity (f b)
  distribute = Identity . fmap runIdentity

instance Settable Identity

type SimpleSetter s a =
  (a -> Identity a) -> s -> Identity s

type LessSimpleSetter s t a b =
  (a -> Identity b) -> s -> Identity t

class (Applicative f, Distributive f, Traversable f) => Settable f

type Setter s t a b =
  forall f. Settable f => (a -> f b) -> s -> f t

set :: Setter s t a b -> b -> s -> t
set s b = runIdentity #. s (\_ -> Identity b)

over :: Setter s t a b -> (a -> b) -> s -> t
over setter f = runIdentity #. setter (Identity #. f)

(+~) :: Num a => Setter s t a a -> a -> s -> t
setter +~ a = over setter (+a)

class Monad m => MonadState s m | m -> s where
  get :: m s
  get = state (\s -> (s, s))

  put :: s -> m ()
  put s = state (\_ -> ((), s))

  state :: (s -> (a, s)) -> m a
  state f = do
    s <- get
    let ~(a, s') = f s
    put s'
    return a

modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

(.=) :: MonadState s m => Setter s s a b -> b -> m ()
setter .= b = modify $ set setter b

(%=) :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
setter %= f = modify $ over setter f
