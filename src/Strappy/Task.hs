-- Task.hs

-- | A task is a function from a combinator to a reward. 
module Strappy.Task where

import Strappy.Expr
import Strappy.CL
import Strappy.Type

type Reward = Double
data Task =  Task {taskName :: String, 
                   task :: (Comb -> Reward),
                   taskType :: Type} 
          | SymRegTask {taskName :: String,
                        taskVals :: [Int],
                        taskType :: Type}
          | DigArithTask {taskName :: String,
                          taskBoolVals :: [Bool],
                          taskCard :: Int,
                          taskType :: Type}

type TaskSet = [Task]

instance Show Task where
    show (Task n _ _ ) = n
    show (SymRegTask n _ _ ) = n
    show (DigArithTask n _ _ _ ) = n


-- | task constructors

mkSingleEqualityTask:: Int -> Int -> Task
mkSingleEqualityTask rlimit i 
    = let f c = fromIntegral $ (abs $ a - i)
              where a  = case  reduceComb c of
                           (N a) -> a
                           otherwise -> maxBound
      in Task (show i) f tInt

showTaskCombAssignments :: [(Task, Comb)] -> String
showTaskCombAssignments = unlines . map f
    where f (t, c) = show t ++ ": " ++ show c

    

