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

main :: IO ()
main = do
  x :: Mat Samples InputNeurons Float <- Mat.random
  y :: Vec InputNeurons (Fin OutputNeurons) <- Vec.random

  -- input dimension: D
  -- hidden layer dimension: H
  -- number of classes: C

  -- first layer weights
  w1 :: Mat InputNeurons HiddenNeurons Float <- Mat.random

  -- first layer biases
  b1 :: Vec HiddenNeurons Float <- Vec.random

  -- second layer weights
  w2 :: Mat HiddenNeurons OutputNeurons Float <- Mat.random

  -- second layer biases
  b2 :: Vec OutputNeurons Float <- Vec.random

  -- forward pass

  -- first layer pre-activation
  let z1 = (x `Mat.mul` w1) `Mat.addVec` b1

  -- first layer activation
  let a1 = z1 <&&> max 0

  -- second layer pre-activation
  let z2 = (a1 `Mat.mul` w2) `Mat.addVec` b2
  let scores = z2

  -- second layer activation
  let exp_scores = scores <&&> exp
  -- the hell is happening here...
  let a2 = Mat.scale (1.0 / Vec.sum (Vec.tabulate \i -> exp_scores ! i ! _)) exp_scores

  return ()

-- main :: IO ()
-- main = do
--   let a :: Vec _ Float
--       a = 0.0 .: 0.0 .: 1.0 .: 1.0 .: Vec.empty

--       b :: Vec _ Float
--       b = 0.0 .: 1.0 .: 0.0 .: 1.0 .: Vec.empty

--       y_xor :: Vec _ Float
--       y_xor = 0.0 .: 1.0 .: 1.0 .: 0.0 .: Vec.empty

--       total_input :: Mat InputNeurons Samples Float
--       total_input =
--         a
--           .: b
--           .: Vec.empty

--       samples :: Int
--       samples = total_input Vec.! 0 & length

--       learning_rate :: Float
--       learning_rate = 0.1

--       iterations :: Int
--       iterations = 1000

--   -- stdgen <- getStdGen
--   let stdgen = mkStdGen 111
--   seed <- newIOGenM stdgen

--   return ()

-- main :: IO ()
-- main = return ()