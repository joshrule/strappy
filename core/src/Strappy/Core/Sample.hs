-- Sample.hs
-- |
-- Module:      Strappy.Core.Sample
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | This module probabilistically samples programs from a grammar.

module Strappy.Core.Sample (sampleExprs) where

-- External imports --
import Control.Arrow (second)
import Control.Exception 
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Maybe
import Control.Monad.Random
import Control.Monad.State
import qualified Data.Map as Map
import Data.Either.Combinators (fromRight')
import Data.Maybe
import Data.String (IsString)

-- Strappy imports --
import Numeric.StatsUtils
import Strappy.Core.Expr
import Strappy.Core.Grammar
import Strappy.Core.Type

type PotentiallyFailedSample m = ErrorT String m 

-- | Wrapper over safeSample that collects many samples into a single frontier.
sampleExprs :: (MonadRandom m) => Int -> Int -> Grammar -> Type -> m (ExprMap Double)
sampleExprs frontierSize maxSamples g tp =
    liftM (Map.mapWithKey reweight) $ foldM accSample Map.empty [1..maxSamples]
  where
    accSample acc _ | Map.size acc >= frontierSize = return acc
    accSample acc x = do
        expr <- safeSample g tp
        return $ Map.insertWith (+) expr 1 acc
    reweight expr cnt = -- weight = log(timesSampled(Expr) * p(expr))
        log (fromIntegral cnt) + (fromRight' (exprLogLikelihood g tp expr))

-- | Wrapper over sampleExpr that recovers from failed samples.
safeSample :: (MonadRandom m) => Grammar -> Type -> m Expr
safeSample gr tp = do
    result <- runErrorT (sampleExpr gr tp)
    case (result) of
        (Left   err)  -> safeSample gr tp -- TODO: log error string here
        (Right  expr) -> return expr

-- | Sample an expression of a requested type from a stochastic grammar.
sampleExpr :: (MonadRandom m) => Grammar -> Type -> PotentiallyFailedSample m Expr
sampleExpr Grammar{grApp=p, grExprDistr=exprDistr} requestedType =
    evalTI $ do
        initializeTI exprDistr
        tp' <- instantiateType requestedType
        sample tp'
  where 
    sample tp = do
        shouldExpand <- lift $ lift $ flipCoin (exp p)
        case shouldExpand of
            True -> do
                t <- mkTVar
                e_left  <- sample (t ->- tp)
                t' <- applySub t
                e_right <- sample t'
                tp' <- applySub tp
                return $ App 
                    { eType = tp'
                    , eLeft = e_left
                    , eRight = e_right }
            False -> do 
                cs <- filterM (\(e, _) -> canUnify tp (eType e)) $
                                 Map.toList exprDistr
                when (null cs) $ lift $ throwError $ strMsg "sampleExpr: Unable to find matching type"
                e <- lift $ lift $ sampleMultinomial $ map (second exp) (normalizeDist cs)
                eTp <- instantiateType (eType e)
                unify eTp tp
                return $ e { eType = tp }
