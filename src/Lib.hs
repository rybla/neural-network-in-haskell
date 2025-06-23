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
import Data.Function
import Data.Functor
import Data.Maybe (fromJust)
import Data.Nat ()
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Language.Haskell.TH
import Math.Common
import Math.Mat (Mat)
import qualified Math.Mat as Mat
import Math.Vec (Vec, (.:))
import qualified Math.Vec as Vec
import Notation
import System.Random
import System.Random.Stateful (newIOGenM)

--------------------------------------------------------------------------------

type Samples1 = [nat|4|]

type InputNeurons = [nat|2|]

type HiddenNeurons = [nat|2|]

type OutputNeurons = [nat|1|]

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

main :: IO ()
main = return ()