-- Parameters.hs
-- |
-- Module:      Strappy.Core.Parameters
-- Copyright:   (c) Eyal Dechter
-- License:     MIT
-- Maintainer:  Eyal Dechter <edechter@mit.edu>
-- Stability:   experimental
--
-- | Dynamic configuration support for Strappy simulations
module Strappy.Parameters (
    -- * Types
    PLabel(..),
    PValue(..),
    Parameters(..),
    -- * Conversion
    loadConfig,
    configToParameters,
    -- * Extraction
    fromPDouble,
    fromPBool,
    fromPRational,
    fromPString,
    fromPTaskSet,
    fromPLibrary
    ) where

import qualified Data.Configurator as C
import Data.Configurator.Types
import qualified Data.Map as Map
import Data.Text

import Strappy.Expr
import Strappy.Task

-- | Parameter keys are strings.
type PLabel = String

-- | Parameter values can be one of several types.
data PValue = PDouble Double
            | PBool Bool
            | PRational Rational
            | PString String
            | PTaskSet TaskSet
            | PLibrary Library

-- | Extract A Double from a PDouble.
fromPDouble :: PValue -> Maybe Double
fromPDouble (PDouble d) = Just d
fromPDouble _           = Nothing

-- | Extract A Bool from a PBool.
fromPBool :: PValue -> Maybe Bool
fromPBool (PBool b) = Just b
fromPBool _         = Nothing

-- | Extract A Rational from a PRational.
fromPRational :: PValue -> Maybe Rational
fromPRational (PRational r) = Just r
fromPRational _             = Nothing

-- | Extract A String from a PString.
fromPString :: PValue -> Maybe String
fromPString (PString s) = Just s
fromPString _           = Nothing

-- | Extract A TaskSet from a PTaskSet.
fromPTaskSet :: PValue -> Maybe TaskSet
fromPTaskSet (PTaskSet t) = Just t
fromPTaskSet _            = Nothing

-- | Extract A Library from a PLibrary.
fromPLibrary :: PValue -> Maybe Library
fromPLibrary (PLibrary l) = Just l
fromPLibrary _            = Nothing

-- | A full set of Parameters is just a key-value map.
type Parameters = Map.Map PLabel PValue

-- | Translate Configurator Values to Parameters PValues.
cValueToPValue :: Maybe Value -> Maybe PValue
cValueToPValue (Just (Bool   b)) = Just (PBool b)
cValueToPValue (Just (String s)) = Just (PString $ unpack s)
cValueToPValue (Just (Number n)) = Just (PRational $ n)
cValueToPValue (Just (List  ls)) = Nothing
cValueToPValue Nothing           = Nothing

-- | Update the defaults with user-specified values.
loadConfig :: Config -> IO Parameters
loadConfig config = configToParameters config defaultParameters

-- | Update a given parameter set with user-specified values.
configToParameters :: Config -> Parameters -> IO Parameters
configToParameters config params =
    let maybeUpdate k p = do
            searchResult <- C.lookup config (pack k)
            return $ maybe p 
                           (\x -> Map.insert k x p)
                           (grAppHack k $ cValueToPValue $ searchResult)
        grAppHack "grApp" (Just (PString s)) = Just (PDouble (read s))
        grAppHack _ x = x
    in return params                     >>= 
       maybeUpdate "writeLog"            >>=
       maybeUpdate "pruneGrammar"        >>=
       maybeUpdate "sampleByEnumeration" >>=
       maybeUpdate "frontierSize"        >>=
       maybeUpdate "frontierSamples"     >>=
       maybeUpdate "maxEvalTime"         >>=
       maybeUpdate "rndSeed"             >>=
       maybeUpdate "lambda"              >>=
       maybeUpdate "pseudocounts"        >>=
       maybeUpdate "prefix"              >>=
       maybeUpdate "grApp"               >>=
       maybeUpdate "nIters"

-- | The default Parameters
defaultParameters :: Parameters
defaultParameters = Map.fromList [
    -- (T) log output (grammars, etc), (F) don't log
    ("writeLog", PBool True),
    -- (T) prune grammar to search more frontier with less accuracy, (F) don't
    ("pruneGrammar", PBool False),
    -- (T) sample by enumeration, (F) sample by true sampling
    ("sampleByEnumeration", PBool True),
    -- Max size of the frontier enumerated during the "E" step
    ("frontierSize", PRational 1000),
    -- Maximum number of samples drawn from the grammar
    ("frontierSamples", PRational 20000),
    -- Timeout for evaluations (in nanoseconds)
    ("maxEvalTime", PRational 10000),
    -- the random seed
    ("rndSeed", PDouble 0),
    -- lambda, the threshold productions must pass to be saved
    ("lambda", PDouble 1),
    -- pseudocounts for productions in the grammar
    ("pseudocounts", PDouble 1),
    -- Probability of an application in the seed grammar
    ("grApp", PDouble (log 0.375)),
    -- The number of iterations to run
    ("nIters", PRational 10),
    -- directory in which to log all output
    ("prefix", PString ""),
    -- the library of primitives in the seed grammar
    ("grLibrary", PLibrary []),
    -- all the tasks Strappy should attempt to hit
    ("tasks", PTaskSet []) ]
