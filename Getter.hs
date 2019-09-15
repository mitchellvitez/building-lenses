{-# LANGUAGE RankNTypes #-}

module Getter where

newtype SimpleGetter s a = SimpleGetter (s -> a)

viewSimple :: SimpleGetter s a -> s -> a
viewSimple (SimpleGetter get) s = get s

(^.$) :: s -> SimpleGetter s a -> a
(^.$) = flip viewSimple

toSimpleGetter :: (s -> a) -> SimpleGetter s a
toSimpleGetter f = SimpleGetter f

lengthCont :: (Int -> r) -> String -> r
lengthCont callback str = callback $ length str

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

(>$<) :: Contravariant f => (a -> b) -> f b -> f a
(>$<) = contramap

newtype MakeString a =
  MakeString { mkString :: a -> String }

instance Contravariant MakeString where
  contramap f (MakeString mkStr) =MakeString (mkStr . f)

type Getter s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

newtype Const a b = Const { getConst :: a }

instance Functor (Const m)
  where fmap _ (Const a) = Const a

instance Contravariant (Const m)
  where contramap _ (Const b) = Const b

(^.) :: s -> Getter s a -> a
s ^. g  = getConst (g Const s)

infixl 8 ^.

view :: Getter s a -> s -> a
view g s = s ^. g
