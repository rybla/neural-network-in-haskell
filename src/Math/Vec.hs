{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Math.Vec where

import Control.Monad
import Data.Data (Proxy (..))
import Data.Fin (Fin)
import Data.Maybe (fromJust)
import Data.Type.Nat (Nat (..), SNatI)
import qualified Data.Type.Nat as Nat
import qualified Data.Vec.Pull as Vec
import System.Random (Random, randomIO)

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

random :: forall n a. (SNatI n, Random a) => IO (Vec n a)
random = fromJust . Vec.fromList <$> replicateM (Nat.reflectToNum (Proxy @n)) randomIO

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

scale :: (Num a) => a -> Vec n a -> Vec n a
scale x v = tabulate \i -> x * v ! i
