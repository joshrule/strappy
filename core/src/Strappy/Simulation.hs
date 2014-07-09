-- Simulation.hs
-- |
-- Module:      Strappy.Simulation
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | A wrapper function for Strappy learning simulations. A single E.C.
-- learning step consists of two parts. First, we generate a set of expressions
-- for each type in the given set of tasks. Then, we use these expressions to
-- generate a new grammar which is hopefully better suited to the task domain.
-- These simulations iterate that process to repeatedly improve the grammar.

module Strappy.Simulation (simulate) where

-- External imports --
import qualified Control.Lens as L
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.State
import qualified Data.ByteString.Lazy as B
import qualified Data.Configurator as C
import qualified Data.Map as M
import           Pipes
import           System.Random

-- Strappy imports --
import Strappy.Core.Expr
import Strappy.Core.Grammar
import Strappy.Core.Library
import Strappy.Core.Task
import Strappy.Generate
import Strappy.Logging
import Strappy.Parameters

-- | A wrapper to run simulations with a single function call.
simulate :: FilePath                -- ^ Config Filename
         -> (Parameters -> TaskSet) -- ^ creates tasks given configuration
         -> Library                 -- ^ seed grammar primitives
         -> (B.ByteString -> IO ()) -- ^ Logging Function
         -> IO ()
simulate c t l logFn = evalLogger (simulation c t l) logFn


-- | A way to run simple learning simulations using Strappy.
simulation :: FilePath                -- ^ configuration file name
           -> (Parameters -> TaskSet) -- ^ creates tasks given configuration
           -> Library                 -- ^ seed grammar primitives
           -> Logger ()
simulation c t l = do
    openLog
    logMessage "Loading Parameters"
    config <- lift $ C.load [C.Required c]
    params <- lift $ loadConfig config
    let tasks' = t params
    let params'= L.set grLibrary l (L.set tasks tasks' params)
    logMessage "Initializing Random Seed"
    let rndGen = mkStdGen (L.view rndSeed params)
    logMessage "Initializing Grammar"
    let seedGrammar = normalizeGrammar $ Grammar 
            { grApp = (L.view grPApp params')
            , grExprDistr = initExprDistr (L.view grLibrary params') }
    logGrammar seedGrammar
    finalGrammar <- evalSimulator (iterateGrammar seedGrammar) params' rndGen
    logGrammar finalGrammar
    closeLog
    return ()

-- | A simulation needs the ability to maintain a configuration, make random
-- decisions, and log output at will.
type Simulator = StateT Parameters (RandT StdGen Logger)

-- | Unwrap the Simulator monad.
evalSimulator s config rndGen = evalRandT (evalStateT s config) rndGen

-- | An Iteration combines a grammar and its generating frontier with
-- analysis of their performance.
data Iteration = Iteration
    { iId :: Int
    , iGrammar  :: Grammar
    , iAnalysis :: ()
    , iFrontier :: ExprDistr
    } deriving (Show)

-- | A wrapper over the actual work of iteratively improving the grammar.
iterateGrammar :: Grammar -> Simulator Grammar
iterateGrammar seed = do 
    n <- grab nIters
    grammar' <- lift $ lift $ loopM seed [1..n] $ 
        \grammar step -> do
            logMessage $ "Test " ++ (show step)
--          ec (Iteration 0 grammarSeed M.empty ())
            return grammar
    return grammar'
  where
    loopM start xs step = foldM step start xs

-- | ec performs one iteration of EM on multitask learning.
ec :: Iteration    -- ^ the previous iteration
   -> Simulator Iteration -- ^ improved grammar, frontier, and so on
ec lastIter = do
    t <- grab tasks
    l <- grab lambda
    p <- grab pseudocounts
    return lastIter
    -- return (Iteration step (normalizeGrammar newGrammar) newFrontiers ())
--  where
    -- lastGrammar = iGrammar lastIter
    -- step = (iId lastIter) + 1
    -- newFrontiers = generateFrontiers lastGrammar
    -- newGrammar = newGrammarFromFrontiers lastGrammar newFrontiers t l p
