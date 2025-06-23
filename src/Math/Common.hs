module Math.Common where

sigmoid :: (Floating a) => a -> a
sigmoid x = 1 / (1 + exp (-x))
