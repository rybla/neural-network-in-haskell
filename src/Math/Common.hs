{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Math.Common where

sigmoid :: (Floating a) => a -> a
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: (Floating a) => a -> a
sigmoid' x = sigmoid x - sigmoid x ^^ (2 :: Integer)

fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 f = fmap (fmap f)

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap2

infixl 4 <$$>

(<&&>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) = flip fmap2

infixr 4 <&&>
