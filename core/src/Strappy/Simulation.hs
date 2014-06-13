-- Simulation.hs
-- |
-- Module:      Strappy.Core.Simulation
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | A wrapper function for Strappy learning simulations
module Strappy.Simulation (
    simulation
    ) where

-- External imports --
import Control.Monad
import qualified Data.Configurator as C
import qualified Data.Map as M
import Data.Maybe
import Pipes
import System.Random

-- Strappy imports --
import Strappy.Expr
import Strappy.Grammar
import Strappy.Library
import Strappy.Parameters
import Strappy.Task

writeToLog :: String -> IO ()
writeToLog = putStrLn

initExprDistr :: Library -> ExprDistr
initExprDistr lib = M.fromList $ zip lib [1..]

loopM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
loopM start xs step = foldM step start xs

-- | A wrapper to run simple learning simulations in Strappy 
simulation :: FilePath -- ^ configuration file name
           -> (Parameters -> TaskSet) -- ^ creates tasks given configuration
           -> Library -- ^ productions for seed grammar
           -> IO ()
simulation configFile tasks library = do
    config <- C.load [C.Required configFile]
    params <- loadConfig config
    let params'= (M.insert "tasks" (PTaskSet $ tasks params)) $ 
                   M.insert "grLibrary" (PLibrary library) params
    let pFind = (params' M.!)
    setStdGen $ mkStdGen $ (round . fromJust . fromPDouble $ pFind "rndSeed")
    let seedGrammar = Grammar { grApp = (fromJust . fromPDouble $ pFind "grApp"),
                                grExprDistr = initExprDistr (fromJust . fromPLibrary $ pFind "grLibrary")}
    writeToLog "Test 0"
    grammar'' <- loopM seedGrammar [1..(round . fromJust . fromPRational $ pFind "nIters")] $ \grammar step -> do
        writeToLog $ "Test " ++ (show step)
--      grammar' <- normalizeGrammar $ doEMIter ((pFind prefix) ++ (show step))
--                                              (pFind tasks)
--                                              (pFind lambda)
--                                              (pFind pseudocounts)
--                                              (pFind frontierSize)
--                                              seedGrammar
--                                              grammar
--      return grammar'
        return grammar
    writeToLog $ "\nGrammar:\n" ++ (show grammar'')
    writeToLog "Finished!"
    return ()
