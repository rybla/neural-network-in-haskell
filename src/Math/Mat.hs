{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Math.Mat where

import Control.Monad
import Data.Data (Proxy (..))
import Data.Fin (Fin)
import Data.Maybe (fromJust)
import Data.Nat (Nat (..))
import Data.Type.Nat (SNatI)
import qualified Data.Type.Nat as Nat
import Math.Vec (Vec, (!), (.:))
import qualified Math.Vec as Vec
import System.Random (Random, randomIO)

-- | Matrix of 'm' rows by 'n' columns
type Mat m n a = Vec m (Vec n a)

random :: forall m n a. (SNatI m, SNatI n, Random a) => IO (Mat m n a)
random =
  (fromJust . Vec.fromList) . map (fromJust . Vec.fromList)
    <$> replicateM (Nat.reflectToNum (Proxy @m)) (replicateM (Nat.reflectToNum (Proxy @n)) randomIO)

fromVec :: Vec n a -> Mat (S Z) n a
fromVec v = v .: Vec.empty

toVec :: Mat (S Z) n a -> Vec n a
toVec m = m ! 0

tabulate :: (Fin m -> Fin n -> a) -> Mat m n a
tabulate f = Vec.tabulate (Vec.tabulate . f)

transpose :: Mat m n a -> Mat n m a
transpose m = tabulate \i j -> m ! j ! i

fill :: a -> Mat m n a
fill a = Vec.fill (Vec.fill a)

mul :: (SNatI n, Num a) => Mat m n a -> Mat n p a -> Mat m p a
mul a b = Vec.tabulate \i -> Vec.tabulate \j -> Vec.sum $ Vec.zipWith (*) (a ! i) (Vec.tabulate \k -> b ! k ! j)

add :: (Num a) => Mat m n a -> Mat m n a -> Mat m n a
add a b = Vec.tabulate \i -> Vec.tabulate \j -> (a ! i ! j) + (b ! i ! j)

sub :: (Num a) => Mat m n a -> Mat m n a -> Mat m n a
sub a b = Vec.tabulate \i -> Vec.tabulate \j -> (a ! i ! j) - (b ! i ! j)

log :: (Floating a) => Mat m n a -> Mat m n a
log = fmap (fmap Prelude.log)
