{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  contramap f (MakeString mkStr) = MakeString (mkStr . f)

type GeneralGetter s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

type Getter s a =
  forall r. (a -> Const r a) -> s -> Const r s

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

-- f :: a -> Const r a
headGetter :: Getter [a] a
headGetter f = Const . getConst . f . head

nestedGetter :: Getter (a, ((b, (c, d)), e)) d
nestedGetter f = Const . getConst . f . snd . snd . fst . snd

everyThirdGetter :: Getter [a] [a]
everyThirdGetter f = Const . getConst . f . everyThird
  where everyThird (_:_:x:xs) = x : everyThird xs
        everyThird _ = []

-- I think you get the pattern...
succGetter :: Getter Integer Integer
succGetter = toGetter succ

-- bit of a special case
idGetter :: Getter a a
idGetter = id

toGetter :: (s -> a) -> Getter s a
toGetter get f = Const . getConst . f . get
