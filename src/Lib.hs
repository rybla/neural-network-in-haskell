{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant $" #-}

module Lib (main) where

import Control.Monad
import Data.Data (Proxy (..))
import Data.Distributive (Distributive (..))
import Data.Function
import Data.Functor
import Data.Maybe (fromJust)
import Data.Nat ()
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Data.Vec.Pull (Vec (..))
import qualified Data.Vec.Pull as Vec
import System.Random
import System.Random.Stateful (StatefulGen, newIOGenM, uniformListM)

(.:) :: a -> Vec n a -> Vec (S n) a
(.:) = Vec.cons

infixr 5 .:

-- | Matrix of 'm' rows by 'n' columns
type Mat m n a = Vec m (Vec n a)

type One = S Z

type Two = S One

type Samples = S (S (S (S Z)))

type InputNeurons = S (S Z)

type HiddenNeurons = S (S Z)

type OutputNeurons = S Z

a :: Vec _ Float
a = 0.0 .: 0.0 .: 1.0 .: 1.0 .: Vec.empty

b :: Vec _ Float
b = 0.0 .: 1.0 .: 0.0 .: 1.0 .: Vec.empty

y_xor :: Vec _ Float
y_xor = 0.0 .: 1.0 .: 1.0 .: 0.0 .: Vec.empty

-- TODO: what does the Two have to be equal to? InputNeurons or something?
total_input :: Mat Two Samples Float
total_input =
  a
    .: b
    .: Vec.empty

samples = total_input Vec.! 0 & length

learning_rate :: Float
learning_rate = 0.1

iterations :: Int
iterations = 1000

--------------------------------------------------------------------------------

randomMat :: forall m n a. (Nat.SNatI m, Nat.SNatI n, Random a) => IO (Mat m n a)
randomMat =
  (fromJust . Vec.fromList) . map (fromJust . Vec.fromList)
    <$> ( replicateM (Nat.reflectToNum (Proxy @HiddenNeurons)) $
            replicateM (Nat.reflectToNum (Proxy @InputNeurons)) $
              randomIO
        )

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x))

dotVec :: (Nat.SNatI n) => Vec n Float -> Vec n Float -> Float
dotVec v1 v2 = Vec.sum $ Vec.zipWith (*) v1 v2

mulMat :: (Nat.SNatI n) => Mat m n Float -> Mat n p Float -> Mat m p Float
mulMat a b = Vec.tabulate \i -> Vec.tabulate \j ->
  Vec.sum $ Vec.zipWith (*) (a Vec.! i) (Vec.tabulate \k -> b Vec.! k Vec.! j)

forward_prop ::
  (Nat.SNatI b, Nat.SNatI a) =>
  Mat a b Float ->
  Mat c a Float ->
  Mat b d Float ->
  ( Mat a d Float,
    Mat a d Float,
    Mat c d Float,
    Mat c d Float
  )
forward_prop w1 w2 x =
  let z1 = mulMat w1 x
      a1 = z1 <&> (<&> sigmoid)
      z2 = mulMat w2 a1
      a2 = z2 <&> (<&> sigmoid)
   in (z1, a1, z2, a2)

main :: IO ()
main = do
  -- stdgen <- getStdGen
  let stdgen = mkStdGen 111
  seed <- newIOGenM stdgen

  w1 <- randomMat @HiddenNeurons @InputNeurons @Float
  w2 <- randomMat @OutputNeurons @HiddenNeurons @Float
  let initialState = ([] :: [Float], w1, w2)
  enumFromTo 0 iterations
    & foldM
      ( \(losses, w1, w2) -> \i -> do
          let (z1, a1, z2, a2) = forward_prop w1 w2 total_input
              loss :: Float
              loss = -(1.0 / (samples & fromIntegral)) * ( dotVec y_xor a2 )

          let w1' = w1
              w2' = w2
          return (_ : losses, w1', w2')
      )
      initialState

  return ()
