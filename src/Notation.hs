{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Notation where

import Control.Monad
import Data.Nat ()
import Data.Type.Nat (Nat (..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Read (readMaybe)

mkNatType :: Int -> TypeQ
mkNatType i | i == 0 = [t|Z|]
mkNatType i | i > 0 = [t|S $(mkNatType (i - 1))|]

readNat :: (MonadFail m, Num a) => String -> m a
readNat s = case readMaybe @Integer s of
  Nothing -> fail $ "expected a positive integer literal, but got: " <> show s
  Just i -> return . fromInteger $ i

nat :: QuasiQuoter
nat =
  QuasiQuoter
    { quoteExp = \s -> do
        i <- readNat s
        z <- [|Z|]
        foldr AppE z . replicate i <$> [|S|],
      quoteType = \s -> do
        i <- readNat s
        z <- [t|Z|]
        foldr AppT z . replicate i <$> [t|S|]
    }
