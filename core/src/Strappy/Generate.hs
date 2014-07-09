-- Generate.hs
-- |
-- Module:      Strappy.Generate
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | This module provides a single interface to three different ways one might
-- generate programs from a probabilistic grammar: true probabilistic sampling,
-- iterative deepening, and program enumeration.
--
module Strappy.Generate (generateFrontiers) where

-- External imports --
import           Control.Monad.Random
import           Control.Monad.State
import qualified Data.List as List

-- Strappy imports --
import Strappy.Core.Grammar
import Strappy.Core.Sample
import Strappy.Core.Task
import Strappy.Core.Type
import Strappy.Parameters

-- | Generate sets of programs matching the requested types from a TaskSet
generateFrontiers :: (MonadRandom m, MonadState Parameters m) => Grammar -> m [(Type,ExprMap Double)]
generateFrontiers grammar = do
    t <- grab tasks
    mapM (generateFrontier grammar) (List.nub (map taskType t))
  where
    generateFrontier :: (MonadRandom m, MonadState Parameters m) => Grammar -> Type -> m (Type,ExprDistr)
    generateFrontier grammar tp = do
        n <- grab nSamples
        g <- grab generator
        i <- grab nIters
        sample <- (sampler g i) n grammar tp
        return (tp, sample)
      where
        sampler g i = case g of
            "probabilisticSampling" -> sampleExprs i
            -- "iterativeDeepening" -> cbIterativeDeepening
            _ -> sampleExprs i
