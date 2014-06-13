-- Example.hs
-- |
-- Module:      Main
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | A toy example to demonstrate task and library creation.
module Main (main) where

-- External Imports --
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Pipes

-- Strappy Imports --
import Strappy.Expr
import Strappy.Library
import Strappy.Parameters
import Strappy.Simulation
import Strappy.Task
import Strappy.Type

-- A small library for boolean learning.
library :: Library
library = cBasicRouters ++ [cNand] ++ cBools

-- Build tasks for boolean learning.
makeTask :: Parameters -> String -> (Bool -> Bool -> Bool) -> Task
makeTask config name f =
    Task { taskName = name,
           task = \e -> 
               let results = [ (x,y,(timeLimitedEval 
                                        (round . fromJust . fromPRational $ config M.! "maxTimeEval")
                                        (e <> (boolToExpr x) <> (boolToExpr y)))) |
                        x <- [True, False], y <- [True, False] ] :: [(Bool,Bool,Maybe Bool)]
               in log . fromIntegral . length $ filter (\ (x,y,r) -> Just (f x y) == r) results,
           taskType = (tBool ->- tBool ->- tBool) }

-- Given a base configuration, finalize the tasks.
tasks :: Parameters -> TaskSet
tasks config = [ (makeTask config "and"  (&&)),
                 (makeTask config "or"   (||)),
                 (makeTask config "xor"  (\x y -> (x && not y) || (y && not x))),
                 (makeTask config "notX" (\x y -> not x)),
                 (makeTask config "notY" (\x y -> not y)) ]

-- | A toy example to demonstrate how easily simulations can be built.
main :: IO ()
main = simulation "./src/Strappy/example-config" tasks library
