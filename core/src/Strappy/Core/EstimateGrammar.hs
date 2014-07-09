-- EstimateGrammar.hs
-- |
-- Module:      Strappy.Core.EstimateGrammar
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | This module implements the EM algorithm for estimating a grammar given a
-- corpus of programs. We use the inside-outside algorithm to estimate the
-- expected counts of each rule on each iteration. See
-- <http://www.cs.columbia.edu/~mcollins/io.pdf> for some notes on EM and
-- inside-outside for production probability estimation.

module Strappy.Core.EstimateGrammar (newGrammarFromFrontiers) where

import           Data.Either.Combinators (fromRight')
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe

import Numeric.StatsUtils
import Strappy.Core.Expr
import Strappy.Core.Task
import Strappy.Core.Type
import Strappy.Core.Grammar

-- | Using a newly generated frontier, estimate a new grammar
newGrammarFromFrontiers :: Grammar            -- ^ the old grammar
                        -> [(Type,ExprDistr)] -- ^ the corpus
                        -> TaskSet            -- ^ our tasks
                        -> Double             -- ^ lambda
                        -> Double             -- ^ pseudocounts
newGrammarFromFrontiers lastGrammar frontiers tasks lambda pseudocounts = 
    if Map.null usefulExprs 
    then lastGrammar
    else estimateGrammar lambda
                         pseudocounts
                         (termOnlyGrammar lastGrammar)
                         (keepFrontiersWithMass perTaskFrontiers)
  where
    perTaskFrontiers = addLL $ sortFrontiersByTask tasks frontiers
    usefulExprs = keepExprsWithMass $ normalizeFrontiers perTaskFrontiers

-- | Iteratively compress the grammar until improvement stops.
estimateGrammar :: Double             -- ^ lambda
                -> Double             -- ^ pseudocounts
                -> Grammar            -- ^ initial grammar
                -> [(Task,ExprDistr)] -- ^ For each task, expr likelihoods
                -> Grammar            -- ^ the new grammar
estimateGrammar lambda pseudocounts g0 corpus =
    if productions g0 == productions g'
    then g'
    else estimateGrammar lambda pseudocounts g' corpus
  where 
    g' = compressWeightedCorpus lambda pseudocounts g0 (weightCorpus corpus g0)

-- | Weight each expression according to its usefulness across all tasks.
weightCorpus :: [(Task,ExprMap (Double, Double))] -> Grammar -> [(Expr, Double)]
weightCorpus corpus grammar = 
    Map.toList $ Map.map exp $ foldl1 (Map.unionWith logSumExp) $ 
        normalizeFrontiers $ replaceW corpus grammar

-- | Find new productions by compressing the corpus.
compressWeightedCorpus :: Double           -- ^ lambda
                       -> Double           -- ^ pseudocounts
                       -> Grammar          -- ^ initial grammar
                       -> [(Expr, Double)] -- ^ weighted corpus
                       -> Grammar
compressWeightedCorpus lambda pseudocounts grammar corpus =
    grammar''
  where 
    subtrees = foldl1 (Map.unionWith (+)) $ map (countSubtrees Map.empty) corpus
    terminals = filter isTerm $ Map.keys $ grExprDistr grammar
    newProductions = compressCorpus lambda subtrees
    productions = newProductions ++ terminals -- TODO: need new type annotation here?
    uniformLogProb = -log (List.genericLength productions)
    grammar'   = Grammar (log 0.5) $ Map.fromList [ (prod, uniformLogProb) | prod <- productions ]
    grammar'' = inoutEstimateGrammar grammar' pseudocounts corpus

countSubtrees :: ExprMap Double -- Map of rules and associated counts.
              -> (Expr, Double) -- the expression and its weight
              -> ExprMap Double
countSubtrees cnt (expr@(App{eLeft=l, eRight=r}),wt) =
    cntR
  where
    cntE = incCount cnt (expr,wt)
    cntL = countSubtrees cntE (l,wt)
    cntR = countSubtrees cntL (r,wt)
countSubtrees cnt _ = cnt

incCount :: ExprMap Double -> (Expr, Double) -> ExprMap Double
incCount cnt (expr@App{},wt) = Map.insertWith (+) expr wt cnt
incCount cnt _               = cnt

-- | Keep only those productions passing a usefulness threshold.
compressCorpus :: Double -> ExprMap Double -> [Expr]
compressCorpus lambda counts =
  map fst $ filter (\(_, c) -> c >= lambda) $ Map.toList counts

-- | Does a log probability have any mass in the distribution?
hasMass :: Double -> Bool
hasMass x = (not (isNaN x)) && (not (isInfinite x)) 

-- | Re-arrange the frontiers to associate tasks with their frontier.
sortFrontiersByTask :: TaskSet -> [Map.Map Expr Double] -> [(Task, Map.Map Expr Double)]
sortFrontiersByTask tasks frontiers = 
    map (\t -> (t, lookupFrontier t)) tasks
    where lookupFrontier tp = fromJust $ Map.lookup (taskType tp) frontiers

-- | Replace the prior
replaceW :: [(Task, ExprMap (Double,Double))] -> Grammar -> [(Task, ExprMap (Double,Double))]
replaceW fs g = map (\(t,eMap) -> Map.mapWithKey (replaceW' t) eMap) fs
  where
    replaceW' t = (\e (w,ll) ->
        (fromRight' (exprLogLikelihood g (taskType t) e),ll))

-- | Instead of just containing the prior, add the likelihood p(t|e) to the map.
addLL :: [(Task, ExprMap Double)] -> [(Task, ExprMap (Double, Double))]
addLL fs = 
    map (\(t,eMap) -> (t, Map.mapWithKey (\e w -> (w,(task t) e)) eMap)) fs

-- | Given each expression's prior and likelihood, normalize the task frontiers.
normalizeFrontiers :: [(Task, ExprMap (Double,Double))] -> [(Task, ExprMap Double)]
normalizeFrontiers frontiers =
    zipWith (\logZ (t,f) -> Map.filter hasMass $ Map.map (subtract logZ) f)
            logZs
            summedFrontiers
    where summedFrontiers = map (Map.map (uncurry (+)) . snd) frontiers
          logZs = map (Map.fold logSumExp (log 0.0)) summedFrontiers

-- | Sum the mass across frontiers and keep expressions that have mass.
keepExprsWithMass :: [(Task, ExprDistr)] -> ExprDistr
keepExprsWithMass frontiers =
    Map.filter hasMass $
               foldl (\acc (_,frontier) -> Map.unionWith logSumExp acc frontier)
                     Map.empty
                     frontiers

-- | Keep the frontiers which have at least one expression with mass.
keepFrontiersWithMass :: [(Task, ExprMap (Double, Double))] -> [ExprMap Double]
keepFrontiersWithMass frontiers = 
    filter (\eMap -> any (hasMass) (Map.elems eMap)) $
           map (Map.map snd $ snd) frontiers
