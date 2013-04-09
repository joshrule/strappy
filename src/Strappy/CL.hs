{-# Language GeneralizedNewtypeDeriving, 
  BangPatterns,
  ExistentialQuantification,
  TypeSynonymInstances,
  FlexibleInstances,
  OverlappingInstances #-}

module Strappy.CL where 

-- standard library imports
import qualified Data.Map as Map
import Data.Map ((!), keys)
import Data.List (union)
import Control.Monad 
import Control.Monad.Trans.Class
import Control.Monad.State
import qualified Data.Sequence as Seq
import Data.Sequence ( (|>), (<|))
import qualified Data.Foldable as Foldable (toList)
import Data.Function (on)

--  local imports
import Strappy.Type
import Strappy.CLError
import Strappy.Expr
                          
--  Define combinators -------
data Comb = CApp {lComb :: Comb, 
                  rComb ::  Comb,
                  cType :: Type,
                  cAppDepth :: Int,
                  cLabel :: Maybe String -- ^ the label of a
                                         -- combinator is a unique
                                         -- name for the subtree
                 }
          | CLeaf {cName :: String,
                   cExpr :: Expr,
                   cType :: Type}
          | CInnerNode {cType :: Type} -- ^ a location in a tree 
          | CHole {cHole :: Hole}

data Domain a = Domain [a]
data Hole = forall a. (Show a, Eq a) =>  Hole {holeType :: Type,
                                         holeDomain :: Domain a} 
instance Show Hole where
    show (Hole t d) = "hole :: " ++ show t

instance Eq Hole where
    (Hole t d) == (Hole t2 d2) = (t == t2) 
instance Ord Hole where
    compare = compare `on` holeType

cDepth :: Comb -> Int
cDepth CLeaf{} = 0
cDepth CApp{cAppDepth=d} = d

mkAppDepth :: Comb -> Comb -> Int
mkAppDepth c1 c2 = 1 + max (cDepth c1) (cDepth c2)

isCLeaf (CLeaf{}) = True
isCLeaf _ = False

type SynthComb = Either String Comb
 
app :: SynthComb -> SynthComb -> SynthComb
app m1 m2 = do c1 <- m1
               c2 <- m2
               case getAppType c1 c2 of
                Left err -> Left err
                Right t -> let d = mkAppDepth c1 c2 
                           in Right $ CApp c1 c2 t d Nothing

app' :: Comb -> Comb -> Comb
app' c1 c2 = case getAppType c1 c2 of
               Left err -> error $ "Error in app' in CL.hs: " ++ err
               Right t -> let d = mkAppDepth c1 c2
                              in CApp c1 c2 t d Nothing

infixl 4 <:>
(<:>) = app

infixl 4 <::>
(<::>) = \m1 c2 ->  m1 <:> (return c2)
                  

instance Show Comb where
    show CApp{lComb=c1, rComb=c2} = "(" ++ show c1 ++ " " ++ show c2 ++ ")"
    show (CLeaf n _ _) = n
    show (CInnerNode t) =  "CTerm: " ++ show t
    show (CHole h) = show h

-- | An alternative to show: if combinator is named and evaluates to a
-- number or bool, show it an an evaluated expressions.
show' (CLeaf n _ _ ) = n
show' c@(CApp{lComb=c1, rComb=c2}) = case reduceComb c of
                             (N i) -> show i
                             (C c) -> show c
                             _     ->  "(" ++ show' c1 ++ " " ++ show' c2 ++ ")"

instance Eq Comb where
    (CApp{lComb=c1, rComb=c2, cAppDepth=dl}) == (CApp{lComb=b1, rComb=b2, cAppDepth=dr}) = (dl == dr) && (c1 == b1) && (c2 == b2)
    (CLeaf{cName=n}) == (CLeaf{cName=m}) = (n==m)
    a == b = False



instance Ord Comb where 
    compare c1 c2 = compare (show c1) (show c2)

num2C :: Int -> Comb 
num2C i = CLeaf (show i) (N i) tInt

dOp2C :: String -> (Int -> Int -> Int) -> Comb
dOp2C opString op = CLeaf opString func (tInt ->-  tInt ->- tInt)
    where func = Func $ \(N !x) -> Func $ \(N !y) -> N $ op x y

bool2C :: Bool -> Comb
bool2C c = CLeaf (show c) (B c) tBool

-- | get type outside type monad
getType :: Comb -> Either String Type
getType (CLeaf _ _ t) = Right t
getType c@(CApp{lComb=c1, rComb=c2}) = getAppType c1 c2
getType c@(CInnerNode t) = Right t
getType (CHole (Hole t _)) = Right t

getAppType :: Comb -> Comb -> Either String Type
getAppType c1 c2  
    = let st = do t1 <- lift (getType c1) >>= freshInst
                  t2 <- lift (getType c2) >>= freshInst
                  t <- newTVar Star
                  case mgu t1 (t2 ->- t) of 
                    Nothing -> lift $ Left $ "Type error: " ++ " unable to unify " 
                               ++ show t1 ++ " and " ++ show t2 
                               ++ " when attempting to apply " 
                              ++  show c1 ++ " to " ++ show c2
                    Just subst -> return $ toType (apply subst t1)
      in liftM fst $ runStateT st 0
                                         

sub s (TVar u) c@(CLeaf _ _ t) = c{cType = apply [(s, TVar u)] (cType c)}
                                      
getTvs (CLeaf _ _ t) = tv t
getTvs (CApp{lComb=cl, rComb=cr}) = getTvs cl `union` getTvs cr

comb2Expr c@(CApp{lComb=c1, rComb=c2}) = App (comb2Expr c1) (comb2Expr c2)
comb2Expr c@(CLeaf _ e _) = e

filterCombinatorsByType :: [Comb] -> Type -> StateT Int [] Comb
filterCombinatorsByType (c:cs) t  
    = do ct <- freshInst (cType c)
         case mgu ct t of
           Just sub -> (return  (c{cType = apply sub ct})) `mplus` rest
           Nothing -> rest
      where rest = filterCombinatorsByType cs t
filterCombinatorsByType [] t = lift []


----------------------------------------------------------------------
-- Reducing combinator -- 

reduceComb :: Comb -> Expr
reduceComb c =  reduce ( comb2Expr c)

fitCombWithHoles :: Comb -> Comb
fitCombWithHoles c = let holes = getHoles c
                     in undefined

data Dir = L | R deriving (Show, Ord, Eq)
type TrPtr = Seq.Seq Dir
instance Show TrPtr where 
    show xs = show (Foldable.toList xs)
 
getHoles :: Comb -> [(Hole, TrPtr)]
-- | getHoles takes a combinator and returns a list of holes and their
-- locations (i.e. pointers of type TrPtr to their locations in a tree).
getHoles c = getHoles' [] Seq.empty c
    where getHoles' holes ptr CApp{lComb=l, rComb=r} 
              = let leftHoles = getHoles' holes (ptr |> L) l
                    rightHoles = getHoles' leftHoles (ptr |> R)  r
                in rightHoles
          getHoles' holes ptr CHole{cHole=h} = (h,ptr):holes
          getHoles' holes _ _ = holes


boolHole :: Hole 
boolHole = Hole tBool (Domain [True, False])



                             

                                  


      



      

