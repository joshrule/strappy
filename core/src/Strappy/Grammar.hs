-- Grammar.hs
-- |
-- Module:      Strappy.Core.Grammar
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | This module defines data types and methods for distributions over
-- expressions. Distributions are specified as weighted grammars.

module Strappy.Grammar (
   -- * Grammar
  Grammar(..),
  normalizeGrammar,
  getUnifyingPrims,
  logLikelihoodExpr,
  -- * ExprMap        
  ExprMap,
  ExprDistr,
  showExprDistr,
     ) where

-- External imports --
import Data.Maybe 
import qualified Data.Map as Map hiding ((\\))
import Data.Set (Set())
import qualified Data.Set as Set
import Data.Hashable
import GHC.Prim
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.List as List
import Data.List ((\\))
import Text.Printf
import Data.Function (on)
import Control.Monad.Identity
import Control.Monad.State
import Control.Arrow (first)
import Debug.Trace
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Directory
import Data.String (IsString)
import Control.Monad.Error.Class

-- Strappy imports -- 
import Strappy.Type
import Strappy.Expr
import Numeric.StatsUtils


-- | Type alias for hash table mapping expressions to values. 
type ExprMap a = Map.Map Expr a

-- | Type alias for distribution over expressions. Values are
-- interpreted as normalized or unnormalized log probabilities.
type ExprDistr = ExprMap Double 

showExprDistr :: ExprDistr -> String
showExprDistr exprDistr  = unlines $ map (\(e, i) -> printf "%7s:%7.2f" (show e) i) pairs
    where pairs = List.sortBy (compare `on` snd) $ Map.toList exprDistr

-- | Type for stochastic grammar over programs. Note that, when
-- normalized, the @ExprDistr@ is interpreted as the distribution over
-- expressions conditioned on an application being requested.
data Grammar = Grammar {grApp :: Double,         -- ^ log probability of application
                        grExprDistr :: ExprDistr -- ^ distribution over functions
                       }

instance Show Grammar where
  show (Grammar p exprDistr) = printf "%7s:%7.2f\n" "p" (exp p) ++ showExprDistr exprDistr

-- | Normalizes both the application and expression distribution
-- fields of the grammar.
normalizeGrammar :: Grammar -> Grammar 
normalizeGrammar gr@Grammar{grExprDistr=distr} =
  let distr' = Map.fromList $ normalizeDist $ Map.toList distr
  in gr { grExprDistr = distr' }


-- | Return the primitives in the grammar that unify with a given
-- type. Expressions and their weights are returned with the current
-- type context.
getUnifyingPrims :: MonadError String m => Grammar -> Type -> [TypeInference m (Expr, Double)]
getUnifyingPrims (Grammar gamma exprDistr) tp = do
  (e, w) <- Map.toList exprDistr
  return $ do unifyExpr tp e
              return (e, w)

-- | Return the loglikelihood of producing the given expression. This
-- is the calculation defined in the Dechter et. al, IJCAI paper.
logLikelihoodExpr :: MonadError String m => Grammar -> Type -> Expr -> TypeInference m Double
logLikelihoodExpr gr@(Grammar gamma _) tp expr = do
  llPrim <- logLikelihoodPrim gr tp expr
  llApp <- logLikelihoodApp gr tp expr
  let logpNoApp = log (1 - exp gamma)
  return $ logSumExp ( logpNoApp + llPrim) (llApp + gamma)

-- TODO: Rewrite using current Expr type
-- -- | the log probability of the given expression under the given grammar.
-- -- This doesn't take into account type information when computing likelihoods
-- pcfgLogLikelihood :: Grammar   -- ^ the source grammar
--                      -> Expr   -- ^ the hypothetically produced expression
--                      -> Double -- ^ the computed log likelihood
-- pcfgLogLikelihood (Grammar { grExprDistr = distr }) e@(Term { }) = e { eLogLikelihood = Just (distr Map.! e) }
-- pcfgLogLikelihood gr@(Grammar { grExprDistr = distr, grApp = app }) e@(App { eLeft = l, eRight = r }) =
--   let l' = pcfgLogLikelihood gr l
--       r' = pcfgLogLikelihood gr r
--       lLL = fromJust $ eLogLikelihood l'
--       rLL = fromJust $ eLogLikelihood r'
--       eLL = logSumExp (app + lLL + rLL)
--                       (case Map.lookup e distr of
--                           Nothing -> log 0.0
--                           Just p -> p)
--   in e { eLeft = l', eRight = r', eLogLikelihood = Just eLL }
  

-- | Return the loglikelihood of returning a given primitive from the
-- library given a current requested type, and conditioned on being
-- asked for a primitive.
logLikelihoodPrim :: MonadError String m => Grammar -> Type -> Expr -> TypeInference m Double
logLikelihoodPrim gr@(Grammar gamma exprDistr) tp expr = do
  (ctx :: Context) <- get
  let loop !acc [] = return $! acc
      loop !acc (x:xs) = do (_, w) <- x
                            loop (w:acc) xs
  w_alts <- lift $ loop [] [evalStateT m ctx | m <- getUnifyingPrims gr tp]
  let logZ = logSumExpList w_alts
  case Map.lookup expr exprDistr  of
    Nothing -> return $! negInfty
    Just ll  -> return $! ll - logZ

  
-- | Return the loglikelihood of returning a given expr from the
-- library given a current requested type, and conditioned on being
-- asked for an application.
logLikelihoodApp :: MonadError String m => Grammar -> Type -> Expr -> TypeInference m Double
logLikelihoodApp _ _ Term{} = return $! negInfty -- probability 0 of getting a terminal
logLikelihoodApp gr@(Grammar gamma exprDistr) tp (App eL eR eTp) = do
  do eta <- mkTVar
     llL <- logLikelihoodExpr gr (eta ->- tp) eL
     llR <- logLikelihoodExpr gr eta eR
     return $ llL + llR

negInfty :: Double
negInfty = read "-Infinity"

