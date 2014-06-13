module Strappy.Task where

import Strappy.Expr
import Strappy.Type

-- | A task is some set of input output pairs governed by a function
data Task = Task { taskName :: String, 
                   task :: Expr -> Double,
                   taskType :: Type} 

type TaskSet = [Task]

instance Show Task where
    show (Task n _ _ ) = n
