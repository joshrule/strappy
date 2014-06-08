-- Expr.hs
{-# Language GADTs,  ScopedTypeVariables, FlexibleInstances, UndecidableInstances   #-}

module Strappy.Expr where


import Unsafe.Coerce (unsafeCoerce) 
import Control.Monad
import Control.Monad.Identity
import Data.Hashable
import Text.Printf
import Control.Exception
import Control.Monad.Error.Class
import System.IO.Unsafe
import Control.Concurrent.Timeout
import Control.DeepSeq (deepseq)
import Data.Maybe
import Data.String (IsString)

import Strappy.Type
import Strappy.Config
import Strappy.Utils

-- | Main data type. Holds primitive functions (Term), their
-- application (App) and annotations.
data Expr = forall a.
            Term {eName  :: String, 
                  eType  :: Type, 
                  eReqType :: Maybe Type,
                  eLogLikelihood :: Maybe Double,
                  eThing :: a }
          | App {eLeft  :: Expr,
                 eRight :: Expr,
                 eType  :: Type,
                 eReqType :: Maybe Type, 
                 eLogLikelihood :: Maybe Double }
             
-- | smart constructor for terms
mkTerm :: forall a. String -> Type -> a -> Expr
mkTerm name tp thing = Term { eName = name,
                              eType = tp, 
                              eReqType = Nothing,
                              eLogLikelihood = Nothing,
                              eThing = thing }

-- | smart constructor for applications
(<>) :: Expr -> Expr -> Expr
a <> b = App { eLeft = a, 
               eRight = b, 
               eType = tp, 
               eLogLikelihood = Nothing,
               eReqType = Nothing }
         where tp = case runTI $ typeOfApp a b of
                Left err -> error err
                Right t -> t

(<.>) :: (IsString e, MonadError e m) => TypeInference m Expr -> TypeInference m Expr -> TypeInference m Expr
ma <.> mb = do a <- ma
               b <- mb
               ta <- instantiateType (eType a)
               tb <- instantiateType (eType b)
               let a' = a{eType=ta}
                   b' = b{eType=tb}
               tp <- typeOfApp a' b'
               return App{eLeft = a', eRight = b', eType = tp, eLogLikelihood = Nothing, eReqType = Nothing}
 

instance Show Expr where
    show Term{eName=s} = s
    show App{eLeft=el, eRight=er} = "(" ++ show el ++ " " ++  show er ++ ")"

showExprLong :: Expr -> String
showExprLong Term{eName=n, eType=tp, eReqType=rt} = printf "%7s, type: %50s, reqType: %50s" 
                                                n (show tp) (show rt)  
showExprLong App{eLeft=l, eRight=r, eType=tp, eReqType=rt }
    = printf ("app, type: %7s, reqType: %7s\n--"++showExprLong l ++ "\n--" ++ showExprLong r ++ "\n")  (show tp)  (show rt)

instance Eq Expr where
  Term { eName = n } == Term { eName = n' } = n == n'
  App { eLeft = l, eRight = r} == App { eLeft = l', eRight = r' } = l == l' && r == r'
  _ == _ = False

instance Ord Expr where
    compare (Term {eName = n}) (Term {eName = n'}) = compare n n' 
    compare (App {}) (Term {}) = LT
    compare (Term {}) (App {}) = GT
    compare (App { eLeft = l, eRight = r }) (App { eLeft = l', eRight = r' }) =
      case compare l l' of
        EQ -> compare r r'
        cmp -> cmp

typeOfApp :: (IsString e, MonadError e m) => Expr -> Expr -> TypeInference m Type
typeOfApp e_left e_right 
    = do tp <- mkTVar
         unify (eType e_left) (eType e_right ->- tp)
         applySub tp

eval :: Expr -> a
-- | Evaluates an Expression of type a into a Haskell object of that
-- corresponding type.
eval Term{eThing=f} = unsafeCoerce f
eval App{eLeft=el, eRight=er} = eval el (eval er)



isTerm :: Expr -> Bool
isTerm Term{} = True
isTerm _ = False

cBottom :: Expr
cBottom = mkTerm "_|_" (TVar 0) (error "cBottom: this should never be called!") 

timeLimitedEval :: Show a => Expr -> Maybe a
timeLimitedEval expr = unsafePerformIO $
                       handle (\(_ :: SomeException) -> return Nothing) $ 
                         timeout maxEvalTime $ do
                           -- Hack to force Haskell to evaluate the expression:
                           -- Convert the (eval expr) in to a string,
                           -- then force each character of the string by putting it in to an unboxed array
                           let val = eval expr
                           forceShowHack val
                           --let strVal = show val
                           -- a <- (newListArray (1::Int,length strVal) strVal) :: IO (IOUArray Int Char)
                           -- strVal `deepseq` return val
                           return val


-- | Runs type inference on the given program, returning its type
doTypeInference :: (IsString e, MonadError e m) => Expr -> m Type
doTypeInference expr = runTI $ doTypeInferenceM expr

doTypeInference_ expr = case doTypeInference expr of 
    Right t -> t
    Left err -> error err

doTypeInferenceM :: (IsString e, MonadError e m) => Expr -> TypeInference m Type
doTypeInferenceM (Term { eType = tp }) = instantiateType tp
doTypeInferenceM (App { eLeft = l, eRight = r }) = do
  alpha <- mkTVar
  beta <- mkTVar
  lTp <- doTypeInferenceM l
  unify lTp (alpha ->- beta)
  rTp <- doTypeInferenceM r
  unify rTp alpha
  applySub beta

typeChecks :: Expr -> Bool
typeChecks expr = case runTI $ doTypeInferenceM expr of 
        Left _ -> False
        Right _ -> True

-- | Folds a monadic procedure over each subtree of a given expression
exprFoldM :: Monad m => (a -> Expr -> m a) -> a -> Expr -> m a
exprFoldM f a e@(Term {}) = f a e
exprFoldM f a e@(App { eLeft = l, eRight = r}) = do
  a'   <- f a e
  a''  <- exprFoldM f a' l
  exprFoldM f a'' r

-- | Returns number of characters in an expression
-- | Does not count both opening and closing parens (these are redundant; see, for example, Unlambda)
exprSize :: Expr -> Int
exprSize Term {} = 1
exprSize App { eLeft = l, eRight = r } = 1 + exprSize l + exprSize r

-- | Substitutes instances of Old for New in Target
subExpr :: Expr -> -- ^ Old
           Expr -> -- ^ New
           Expr -> -- ^ Target
           Expr    -- ^ Updated target
subExpr _ _ e@(Term { }) = e
subExpr old new target | target == old = new
subExpr old new e@(App { eLeft = l, eRight = r }) =
  e { eLeft = subExpr old new l,
      eRight = subExpr old new r }

-- | Procedures for managing holes:
-- | Returns the number of holes in an expression
countHoles :: Expr -> Int
countHoles (App { eLeft = l, eRight = r }) = countHoles l + countHoles r
countHoles (Term { eName = "H" }) = 1
countHoles (Term {}) = 0

-- | Samples values for the holes
sampleHoles :: Expr -> IO Expr
sampleHoles (Term { eName = "H" }) = do
  h <- sampleMultinomial [(0.0, 0.1::Double), (0.1, 0.1), (0.2, 0.1),
                          (0.3, 0.1), (0.4, 0.1), (0.5, 0.1),
                          (0.6, 0.1), (0.7, 0.1), (0.8, 0.1),
                          (0.9, 0.1)]
  return $ cDouble2Expr h
sampleHoles e@(App { eLeft = l, eRight = r}) = do
  l' <- sampleHoles l
  r' <- sampleHoles r
  return $ e { eLeft = l', eRight = r' }
sampleHoles e = return e

-- | Does a monte carlo estimate of the expected likelihood of a probabilistic program
expectedLikelihood :: (Expr -> IO Double) -> Int -> Expr -> IO Double
expectedLikelihood ll _ e | countHoles e == 0 = ll e
expectedLikelihood ll samples e = do
  lls <- replicateM samples (sampleHoles e >>= ll)
  let retval = logSumExpList lls - log (fromIntegral samples)
  return retval

----------------------------------------
-- Conversion functions ----------------
----------------------------------------

showableToExpr :: (Show a) => a -> Type -> Expr
-- | Convert any Showable Haskell object into an Expr.
showableToExpr f tp = mkTerm (show f) tp f

intListToExpr :: [Int] -> Expr
intListToExpr s = showableToExpr s (tList tInt)

stringToExpr :: String -> Expr
stringToExpr s = showableToExpr s (tList tChar)

doubleToExpr :: Double -> Expr
doubleToExpr d = showableToExpr d tDouble

intToExpr :: Int -> Expr
intToExpr d = showableToExpr d tInt

cInt2Expr :: Int -> Expr
-- | Convert integers to expressions. 
cInt2Expr i = mkTerm (show i) tInt i 

cDouble2Expr :: Double -> Expr
-- | Convert doubles to expressions. 
cDouble2Expr i = mkTerm (show i) tDouble i 

cChar2Expr :: Char -> Expr
cChar2Expr c = mkTerm (show c) tChar c

-------------------------------------------
-- Pattern matching for combinators -------
-------------------------------------------
-- ? matches any expression
-- ?t matches terminals
-- ?a matches applications
-- ?d matches deterministic computations (no holes)
matchExpr :: Expr -> -- ^ Pattern
             ([Expr] -> Expr) -> -- ^ Callback 
             Expr -> Expr -- ^ Performs matching
matchExpr pat proc e =
  maybe e proc $ match pat e
  where match (Term { eName = "?" }) t = return [t]
        match (Term { eName = "?t" }) t@(Term {}) = return [t]
        match (Term { eName = "?a" }) a@(App {}) = return [a]
        match (Term { eName = "?d" }) d =
          if countHoles d > 0 then Nothing else return [d]
        match (App { eLeft = l, eRight = r }) (App { eLeft = l', eRight = r' }) = do
          ls <- match l l'
          rs <- match r r'
          return $ ls ++ rs
        match (Term { eName = n }) (Term { eName = n' }) | n == n' = return []
        match _ _ = Nothing

cBool2Expr :: Bool -> Expr
cBool2Expr b = mkTerm (show b) tBool b


getArity :: (IsString e, MonadError e m) =>  Expr -> m Int
getArity expr =
  liftM arity $ doTypeInference expr
  where arity (TCon "->" [t1, t2]) = 1 + arity t2
        arity _ = 0

----------------------------------------
-- Hashable instance ------------------- 
----------------------------------------
instance Hashable Expr where
    hashWithSalt a (Term { eName = name }) = hash a `hashWithSalt` hash name 
                                                  
    hashWithSalt a (App { eLeft = left, eRight = right }) = 
      hash a `hashWithSalt` hash left `hashWithSalt` hash right

----------------------------------------
-- Expressable typeclass -------------- 
----------------------------------------
class Expressable a where
       toExpr :: a -> Expr

instance (Show a, Typeable a) => Expressable a where
       toExpr v = mkTerm (show v) (typeOf v) v 



