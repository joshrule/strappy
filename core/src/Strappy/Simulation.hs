-- Simulation.hs
-- |
-- Module:      Strappy.Simulation
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | A wrapper function for Strappy learning simulations
module Strappy.Simulation (simulate) where

-- External imports --
import qualified Control.Lens as L
import           Control.Monad
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
-- import Strappy.Iteration
import Strappy.Logging
import Strappy.Parameters

-- | A wrapper to run simulations with a single function call.
simulate :: FilePath                -- ^ Config Filename
         -> (Parameters -> TaskSet) -- ^ creates tasks given configuration
         -> Library                 -- ^ seed grammar primitives
         -> (B.ByteString -> IO ()) -- ^ Logging Function
         -> IO ()
simulate c t l logFn = runEffect $ for (simulation c t l) (lift . logFn)

-- | Build an initial distribution for the seed grammar.
initExprDistr :: Library -> ExprDistr
initExprDistr lib = M.fromList $ Prelude.zip lib [1,1..]

-- | Rearrange the foldM arguments to act more like a loop.
loopM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
loopM start xs step = foldM step start xs

-- | A way to run simple learning simulations using Strappy.
simulation :: FilePath                -- ^ configuration file name
           -> (Parameters -> TaskSet) -- ^ creates tasks given configuration
           -> Library                 -- ^ seed grammar primitives
           -> Producer B.ByteString IO ()
simulation c t l = do
    openLog
    logMessage "Loading Parameters"
    config <- lift $ C.load [C.Required c]
    params <- lift $ loadConfig config
    let tasks' = t params
    let params'= L.set grLibrary l (L.set tasks tasks' params)
    logMessage "Initializing Random Seed"
    lift $ setStdGen $ mkStdGen (L.view rndSeed params)
    let seedGrammar = normalizeGrammar $
         Grammar {Strappy.Core.Grammar.grApp = 
                      (L.view Strappy.Parameters.grApp params'),
                  grExprDistr = initExprDistr 
                      (L.view grLibrary params') }
    grammar'' <- loopM seedGrammar [1..(L.view nIters params')] $ 
        \grammar step -> do
            logMessage $ "Test " ++ (show step)
            return grammar
--          grammar' <- ec config (Iteration 0 seedGrammar M.empty ())
--          return grammar'
    logGrammar grammar''
    closeLog
    return ()
