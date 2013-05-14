{-# Language TypeSynonymInstances, FlexibleInstances #-}

module Strappy.Library where

import qualified Data.HashMap as Map
import Data.Hashable
import GHC.Prim
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.List as List
import Text.Printf
import Data.Function (on)
import Control.Monad.Identity
import Debug.Trace

import Strappy.Type
import Strappy.Expr
import Strappy.Utils

-- | Type alias for hash table with keys as type-hidden expressions.
type ExprMap a = Map.Map UExpr a

instance Hashable UExpr where
    hashWithSalt a  uexpr = hash a `hashWithSalt` (hash $ fromUExpr uexpr)

-- | Type alias for distribution over expressions. 
type ExprDistr = ExprMap Double 

 
showExprDistr exprDistr  = unlines $ map (\(e, i) -> printf "%7s:%7.2f" (show e) i) pairs
    where pairs = List.sortBy (compare `on` snd) $ Map.toList exprDistr
                  

-- | Type for stochastic grammar over programs.
data Grammar = Grammar {grApp :: Double, -- ^ log probability of application
                        grExprDistr :: ExprDistr -- ^ distribution over functions
                       } 

showGrammar (Grammar p exprDistr) = printf "%7s:%7.2f\n" "p" p ++ showExprDistr exprDistr

normalizeGrammar :: Grammar -> Grammar 
normalizeGrammar Grammar{grApp=p, grExprDistr=distr}
    = let logTotalMass = logsumexp $ p : Map.elems distr
          distr' = Map.map (\x -> x - logTotalMass) distr
          p' = p - logTotalMass
      in Grammar  p' distr'

-- | Methods for calculating the loglikelihood of an expression draw from grammar
exprLogLikelihood :: Grammar -> Expr a -> Double
-- | Returns the log probability of producing the given expr tree
exprLogLikelihood gr expr = let e = toUExpr expr in
    -- | Is this expr a leaf?
    if Map.member e (grExprDistr gr)
        then calcLogProb gr expr (maybe (eType expr) id (eReqType expr)) +
            (log $ 1 + (negate $ exp $ negate $ grApp gr))
        else case expr of
            App{eLeft=l, eRight = r} -> exprLogLikelihood gr l +
                                        exprLogLikelihood gr r -
                                        grApp gr
            _  -> error $ "Expression "++show e++" is not an application, and is not in the library."                            

calcLogProb :: Grammar -> Expr a -> Type -> Double
-- | Returns the log probability of using the given expression for the
-- given requesting type.
calcLogProb gr@Grammar{grExprDistr=distr} expr tp 
    = let m = fst . runIdentity . runTI $ filterExprsByType (Map.toList distr) tp
          logp_e = (trace $ show m) $ distr Map.! (toUExpr expr)
          logp_e_tot = logsumexp (map snd m) 
      in (trace $ show logp_e_tot) $ logp_e - logp_e_tot

-- | Helper for turning a Haskell type to Any. 
mkAny :: a -> Any
mkAny x = unsafeCoerce x 

--  Some basic library entires. 
t = mkTVar 0                  
t1 = mkTVar 1                  
t2 = mkTVar 2                  
t3 = mkTVar 3                  

-- | Basic combinators
cI = Term "I" (t ->- t) Nothing id

cS = Term "S" (((t2 ->- t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t)) Nothing $ \f g x -> (f x) (g x)

cB = Term "B" ((t1 ->- t) ->- (t2 ->- t1) ->- t2 ->- t) Nothing $ \f g x -> f (g x)

cC = Term "C" ((t1 ->- t2 ->- t) ->- t2 ->- t1 ->- t) Nothing $ \f g x -> (f x) g 

cBottom = Term "_|_" t Nothing $ undefined


-- | Integer arithmetic
cPlus :: Expr (Int -> Int -> Int)
cPlus = Term "+" (tInt ->- tInt ->- tInt) Nothing (+)

cTimes :: Expr (Int -> Int -> Int)
cTimes = Term "*" (tInt ->- tInt ->- tInt) Nothing (*)

cMinus :: Expr (Int -> Int -> Int)
cMinus = Term "-" (tInt ->- tInt ->- tInt) Nothing (-)

cMod :: Expr (Int -> Int -> Int)
cMod = Term "-" (tInt ->- tInt ->- tInt) Nothing  (-)

cRem :: Expr (Int -> Int -> Int)
cRem = Term "rem" (tInt ->- tInt ->- tInt) Nothing mod

-- | Lists
cCons = Term ":"  (t ->- TAp tList t ->- TAp tList t)  Nothing (:)
cAppend = Term "++" (TAp tList t ->- TAp tList t ->- TAp tList t) Nothing (++)
cHead = Term "head" (TAp tList t ->- t) Nothing head
cMap = Term "map" ((t ->- t1) ->- TAp tList t ->- TAp tList t1) Nothing map
cEmpty = Term "[]" (TAp tList t) Nothing []
cSingle = Term "single" (t ->- TAp tList t) Nothing $ \x -> [x]
cRep = Term "rep" (tInt ->- t ->- TAp tList t) Nothing $ \n x -> take n (repeat x)
cFoldl = Term "foldl" ((t ->- t1 ->- t) ->- t ->- (TAp tList t1) ->- t) Nothing $ List.foldl'
cInts =  [cInt2Expr i | i <- [1..10]]
cDoubles =  [cDouble2Expr i | i <- [1..10]]

-- | A basic collection of expressions
basicExprs :: [UExpr]
basicExprs = [toUExpr cI, 
              toUExpr cS, 
              toUExpr cB, 
              toUExpr cC, 
              toUExpr cBottom,
              toUExpr cTimes, 
              toUExpr cCons, 
              toUExpr cEmpty,
              toUExpr cAppend,
              --         toUExpr cHead,
              toUExpr cMap,
              toUExpr cFoldl,
              toUExpr cSingle,
              toUExpr cRep
             ] 
             ++ map toUExpr cInts

-- | A basic expression distribution
basicExprDistr :: ExprDistr
basicExprDistr = Map.adjust (const (-5)) (toUExpr cBottom) 
                 $ Map.fromList [(e, 1) | e <- basicExprs] 

basicGrammar :: Grammar
basicGrammar = normalizeGrammar $ Grammar 3 basicExprDistr


-- library = Library 0.3 exprs
