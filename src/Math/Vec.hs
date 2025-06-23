{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Math.Vec where

import Data.Fin (Fin)
import Data.Type.Nat (Nat (..), SNatI)
import qualified Data.Vec.Pull as Vec

type Vec = Vec.Vec

(!) :: Vec n a -> Fin n -> a
(!) = (Vec.!)

infixl 9 !

empty :: Vec Z a
empty = Vec.empty

fill :: forall (n :: Nat) a. a -> Vec n a
fill a = Vec.tabulate (const a)

(.:) :: a -> Vec n a -> Vec (S n) a
(.:) = Vec.cons

fromList :: (SNatI n) => [a] -> Maybe (Vec n a)
fromList = Vec.fromList

tabulate :: (Fin n -> a) -> Vec n a
tabulate = Vec.tabulate

infixr 5 .:

dot :: (SNatI n, Num a) => Vec n a -> Vec n a -> a
dot v1 v2 = Vec.sum $ Vec.zipWith (*) v1 v2

sum :: (SNatI n, Num a) => Vec n a -> a
sum = Vec.sum

sub :: (Num a) => Vec n a -> Vec n a -> Vec n a
sub v w = tabulate \i -> v ! i - w ! i

add :: (Num a) => Vec n a -> Vec n a -> Vec n a
add v w = tabulate \i -> v ! i + w ! i

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith = Vec.zipWith
