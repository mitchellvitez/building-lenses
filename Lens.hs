{-# LANGUAGE RankNTypes #-}

module Lens where

import Getter
import Setter

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type SimpleLens s a = Lens s s a a

makeLens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
makeLens get set transform structure =
  set structure <$> transform (get structure)

-- Fun bonus exercise #21: implement makeLens using Getter and Setter instead of plain functions
makeLensCumulative :: Getter s a -> Setter s t a b -> Lens s t a b
makeLensCumulative getter setter = undefined

(%%~) :: Functor f => Lens s t a b -> (a -> f b) -> s -> f t
(%%~) = id
