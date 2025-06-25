-- inspired by: https://mlnotebook.github.io/post/neuralnetwork/#nnarchitecture
-- inspired by: https://ljvmiranda921.github.io/notebook/2017/02/17/artificial-neural-networks/
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant bracket" #-}

module Lib (main) where

import Control.Monad
import Data.Data (Proxy (..))
import Data.Fin (Fin)
import Data.Function
import Data.Functor
import Data.Maybe (fromJust)
import Data.Nat ()
import Data.Type.Nat (Nat (..), SNatI)
import qualified Data.Type.Nat as Nat
import Language.Haskell.TH
import Math.Common
import Math.Mat (Mat)
import qualified Math.Mat as Mat
import Math.Vec (Vec, (!), (.:))
import qualified Math.Vec as Vec
import Notation
import System.Random
import System.Random.Stateful (newIOGenM)
import Prelude hiding (error)

--------------------------------------------------------------------------------

-- | Activate a node
-- - `x`: input nodes
-- - `w`: weights of connections between input and output nodes
-- - `j`: index of output node
activate :: (SNatI x, Num a) => Vec x a -> Mat x y a -> Fin y -> a
activate x w j = Vec.sum $ Vec.tabulate \i -> x ! i * w ! i ! j

-- -- | Error of result
-- -- - `r`: result
-- -- - `t`: target
-- error :: (Fractional a, SNatI y) => Vec y a -> Vec y a -> a
-- error r t = 0.5 * Vec.sum (Vec.tabulate \k -> (r ! k - t ! k))

-- -- | Derivative of error of result
-- -- - `r`: result
-- -- - `t`: target
-- -- - `j`: index of output node
-- -- - `k`: index of input node
-- error'_output :: (Num a) => Vec y a -> Vec y a -> Fin y -> Fin y -> a
-- error'_output r t j k = r ! j * delta
--   where
--     delta = activate _ _ j * (1 - r ! k) * (r ! k - t ! k)

-- error'_hidden

--------------------------------------------------------------------------------

-- -- | Feed-forward pass
-- -- - `x_input`: input
-- -- - `w_hidden`: weights of connections between input and hidden nodes
-- -- - `w_output`: weights of connections between hidden and output nodes
-- forward ::
--   (Floating a, SNatI input, SNatI hidden) =>
--   Vec input a ->
--   Mat input hidden a ->
--   Mat hidden output a ->
--   (Vec hidden a, Vec output a)
-- forward x_input w_hidden w_output =
--   let x_hidden = Vec.tabulate $ sigmoid . activate x_input w_hidden
--       x_output = Vec.tabulate $ sigmoid . activate x_hidden w_output
--    in (x_hidden, x_output)

-- -- | Backward pass
-- -- - `x_input`: input
-- -- - `w_hidden`: weights of connections between input and hidden nodes
-- -- - `w_output`: weights of connections between hidden and output nodes
-- -- - `x_hidden`: output of hidden layer
-- -- - `x_output`: output of output layer
-- -- - `t`: target
-- backward x_input w_hidden w_output x_hidden x_output t =
--   let delta_output k = x_output ! k * (1 - x_output ! k) * (x_output ! k - t ! k)
--       delta_hidden j =
--         x_hidden ! j
--           * (1 - x_hidden ! j)
--           * Vec.sum (Vec.tabulate \k -> delta_output k * w_hidden ! j ! k)
--    in _

--------------------------------------------------------------------------------

type Samples = [nat|4|]

type InputNeurons = [nat|2|]

type HiddenNeurons = [nat|2|]

type OutputNeurons = [nat|1|]

learning_rate = 0.1

n_samples :: forall a. (Num a) => a
n_samples = Nat.reflectToNum (Proxy @Samples)

target_func :: Float -> Float -> Fin OutputNeurons
target_func = undefined

main :: IO ()
main = do
  x :: Mat Samples InputNeurons Float <- Mat.random
  y :: Vec Samples (Fin OutputNeurons) <- undefined -- TODO: correct label for features

  -- input dimension: D
  -- hidden layer dimension: H
  -- number of classes: C

  -- first layer weights
  w1 :: Mat InputNeurons HiddenNeurons Float <- Mat.random

  -- first layer biases
  b1 :: Vec HiddenNeurons Float <- Vec.random

  -- output of first layer is w1*x + b1

  -- second layer weights
  w2 :: Mat HiddenNeurons OutputNeurons Float <- Mat.random

  -- second layer biases
  b2 :: Vec OutputNeurons Float <- Vec.random

  -- output of second layer is w2*(w1*x + b1) + b2
  -- output of second layer is (w2*w1*x :: Input x Output) + (w2*b1 :: Output) + (b2 :: Output)
  -- so might as well have one layer

  -- forward pass

  -- first layer pre-activation
  let z1 = (x `Mat.mul` w1) `Mat.addVec` b1

  -- first layer activation
  let relu = (<&&> max 0)
  -- in this case, relu is the "activation function"
  let a1 = relu z1

  -- second layer pre-activation
  let z2 = (a1 `Mat.mul` w2) `Mat.addVec` b2

  -- second layer activation
  let z2_exp = z2 <&&> exp
  let sums_of_exps = Vec.tabulate \i -> Vec.sum (z2_exp ! i)
  let a2 = Mat.tabulate \i j -> (z2_exp ! i ! j) / sums_of_exps ! i

  -- a2 corresponds to probabilities assigned to each output neuron (which each correspond to a classification label)

  {-
  cross-entropy loss: p log (phat) + q log (1 - phat)
  where p = true label (indicator -- 1 => is label #0; 0 => isnt label #0)
        phat = predicted probability that the input is labeled by label #0
        q = true label (indicator -- 1 => is label #1; 0 => isnt label #1)
        qhat = predicted probability that the input is labeled by label #1
        qhat = 1 - phat
  -}

  let correct_logprobs = Vec.tabulate \i -> -log (a2 ! i ! (y ! i))
  let data_loss = Vec.sum correct_logprobs / n_samples

  -- how much to weight regularization loss vs data loss
  let reg = 1

  -- do regularization to penalize spiky (really big) weights
  let l2_loss :: (SNatI m, SNatI n) => Mat m n Float -> Float
      l2_loss w = Vec.sum (Vec.sum <$> (Mat.tabulate \i j -> (w ! i ! j) ^^ 2))
  let reg_loss = (0.5 * reg * l2_loss w1) + (0.5 * reg * l2_loss w2)

  let loss = data_loss + reg_loss

  -- backward pass

  let d_a2 :: Mat Samples OutputNeurons Float
      d_a2 = Mat.tabulate \i j ->
        ( if j == y ! i
            then a2 ! i ! j - 1
            else a2 ! i ! j
        )
          / n_samples

  let grad_w2 =
        let m = Mat.transpose a1 `Mat.mul` d_a2
         in Mat.tabulate \i j -> m ! i ! j + reg * w2 ! i ! j
  let grad_b2 = Vec.tabulate \i -> Vec.sum (Vec.tabulate \j -> d_a2 ! j ! i)

  let d_hidden =
        let m = d_a2 `Mat.mul` Mat.transpose w2
         in Mat.tabulate \i j ->
              if a1 ! i ! j <= 0
                then 0
                else m ! i ! j

  let grad_w1 =
        let m = Mat.transpose x `Mat.mul` d_hidden
         in Mat.tabulate \i j -> m ! i ! j + reg * w1 ! i ! j
  let grad_b1 = Vec.tabulate \i -> Vec.sum (Vec.tabulate \j -> d_hidden ! j ! i)

  let w1' = Mat.tabulate \i j -> w1 ! i ! j - learning_rate * grad_w1 ! i ! j
  let b1' = Vec.tabulate \i -> b1 ! i - learning_rate * grad_b1 ! i
  let w2' = Mat.tabulate \i j -> w2 ! i ! j - learning_rate * grad_w2 ! i ! j
  let b2' = Vec.tabulate \i -> b2 ! i - learning_rate * grad_b2 ! i

  return ()

step ::
  ( Mat Samples InputNeurons Float,
    Vec Samples (Fin OutputNeurons)
  ) ->
  (Mat InputNeurons HiddenNeurons Float, Vec HiddenNeurons Float, Mat HiddenNeurons OutputNeurons Float, Vec OutputNeurons Float) ->
  (Mat InputNeurons HiddenNeurons Float, Vec HiddenNeurons Float, Mat HiddenNeurons OutputNeurons Float, Vec OutputNeurons Float)
step (x, y) (w1, b1, w2, b2) =
  -- forward pass

  -- first layer pre-activation
  let z1 = (x `Mat.mul` w1) `Mat.addVec` b1

      -- first layer activation
      relu = (<&&> max 0)
      -- in this case, relu is the "activation function"
      a1 = relu z1

      -- second layer pre-activation
      z2 = (a1 `Mat.mul` w2) `Mat.addVec` b2

      -- second layer activation
      z2_exp = z2 <&&> exp
      sums_of_exps = Vec.tabulate \i -> Vec.sum (z2_exp ! i)
      a2 = Mat.tabulate \i j -> (z2_exp ! i ! j) / sums_of_exps ! i

      -- a2 corresponds to probabilities assigned to each output neuron (which each correspond to a classification label)

      {-
      cross-entropy loss: p log (phat) + q log (1 - phat)
      where p = true label (indicator -- 1 => is label #0; 0 => isnt label #0)
            phat = predicted probability that the input is labeled by label #0
            q = true label (indicator -- 1 => is label #1; 0 => isnt label #1)
            qhat = predicted probability that the input is labeled by label #1
            qhat = 1 - phat
      -}

      correct_logprobs = Vec.tabulate \i -> -log (a2 ! i ! (y ! i))
      data_loss = Vec.sum correct_logprobs / n_samples

      -- how much to weight regularization loss vs data loss
      reg = 1

      -- do regularization to penalize spiky (really big) weights
      l2_loss :: (SNatI m, SNatI n) => Mat m n Float -> Float
      l2_loss w = Vec.sum (Vec.sum <$> (Mat.tabulate \i j -> (w ! i ! j) ^^ 2))

      reg_loss = (0.5 * reg * l2_loss w1) + (0.5 * reg * l2_loss w2)

      loss = data_loss + reg_loss

      -- backward pass

      d_a2 :: Mat Samples OutputNeurons Float
      d_a2 = Mat.tabulate \i j ->
        ( if j == y ! i
            then a2 ! i ! j - 1
            else a2 ! i ! j
        )
          / n_samples

      grad_w2 =
        let m = Mat.transpose a1 `Mat.mul` d_a2
         in Mat.tabulate \i j -> m ! i ! j + reg * w2 ! i ! j
      grad_b2 = Vec.tabulate \i -> Vec.sum (Vec.tabulate \j -> d_a2 ! j ! i)

      d_hidden =
        let m = d_a2 `Mat.mul` Mat.transpose w2
         in Mat.tabulate \i j ->
              if a1 ! i ! j <= 0
                then 0
                else m ! i ! j

      grad_w1 =
        let m = Mat.transpose x `Mat.mul` d_hidden
         in Mat.tabulate \i j -> m ! i ! j + reg * w1 ! i ! j
      grad_b1 = Vec.tabulate \i -> Vec.sum (Vec.tabulate \j -> d_hidden ! j ! i)

      w1' = Mat.tabulate \i j -> w1 ! i ! j - learning_rate * grad_w1 ! i ! j
      b1' = Vec.tabulate \i -> b1 ! i - learning_rate * grad_b1 ! i
      w2' = Mat.tabulate \i j -> w2 ! i ! j - learning_rate * grad_w2 ! i ! j
      b2' = Vec.tabulate \i -> b2 ! i - learning_rate * grad_b2 ! i
   in (w1', b1', w2', b2')
