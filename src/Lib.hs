-- inspired by: https://www.digitalocean.com/community/tutorials/constructing-neural-networks-from-scratch
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
import Data.Function
import Data.Functor
import Data.Maybe (fromJust)
import Data.Nat ()
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Math.Common
import Math.Mat (Mat)
import qualified Math.Mat as Mat
import Math.Vec (Vec, (.:))
import qualified Math.Vec as Vec
import System.Random
import System.Random.Stateful (newIOGenM)

--------------------------------------------------------------------------------

forward_prop ::
  Mat m n a ->
  Mat l m a ->
  Mat n l a ->
  ( Mat m l a,
    Mat m l a,
    Mat l l a,
    Mat l l a
  )
forward_prop w1 w2 x =
  let z1 = Mat.mul w1 x
      a1 = z1 <&> (<&> sigmoid)
      z2 = Mat.mul w2 a1
      a2 = z2 <&> (<&> sigmoid)
   in (z1, a1, z2, a2)

--------------------------------------------------------------------------------

type Samples = S (S (S (S Z)))

type InputNeurons = S (S Z)

type HiddenNeurons = S (S Z)

type OutputNeurons = S Z

main :: IO ()
main = do
  let a :: Vec _ Float
      a = 0.0 .: 0.0 .: 1.0 .: 1.0 .: Vec.empty

      b :: Vec _ Float
      b = 0.0 .: 1.0 .: 0.0 .: 1.0 .: Vec.empty

      y_xor :: Vec _ Float
      y_xor = 0.0 .: 1.0 .: 1.0 .: 0.0 .: Vec.empty

      total_input :: Mat InputNeurons Samples Float
      total_input =
        a
          .: b
          .: Vec.empty

      samples :: Int
      samples = total_input Vec.! 0 & length

      learning_rate :: Float
      learning_rate = 0.1

      iterations :: Int
      iterations = 1000

  -- stdgen <- getStdGen
  let stdgen = mkStdGen 111
  seed <- newIOGenM stdgen

  w1 <- Mat.random @HiddenNeurons @InputNeurons @Float
  w2 <- Mat.random @OutputNeurons @HiddenNeurons @Float
  void $
    enumFromTo 0 iterations
      & foldM @_ @_ @([Float], Mat HiddenNeurons InputNeurons Float, Mat OutputNeurons HiddenNeurons Float)
        ( \(losses, w1, w2) i -> do
            let (z1, a1, z2, a2) = forward_prop w1 w2 total_input

                -- loss = -(1/samples) * np.sum( y_xor * np.log(a2)  +  (1-y_xor) * np.log(1-a2) )

                comp1 :: Vec OutputNeurons Float
                comp1 = Mat.toVec (Mat.fromVec y_xor `Mat.mul` Mat.log a2)

                comp2 :: Vec OutputNeurons Float
                comp2 = Mat.toVec $ Mat.fromVec (Vec.fill 1 `Vec.sub` y_xor) `Mat.mul` Mat.log (Mat.fill 1.0 `Mat.sub` a2)

                loss :: Float
                loss = (-(1.0 / (samples & fromIntegral))) * Vec.sum (comp1 `Vec.add` comp2)

                -- TODO: backprop
                -- da2, dw2, dz1, dw1 = back_prop(samples, w1, w2, z1, a1, z2, a2, y_xor)
                -- w2 = w2-lr*dw2
                -- w1 = w1-lr*dw1

                w1' = w1
                w2' = w2
            return (loss : losses, w1', w2')
        )
        ([] :: [Float], w1, w2)
