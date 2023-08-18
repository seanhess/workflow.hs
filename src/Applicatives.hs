module Applicatives where

data A = A deriving (Show)
data B = B deriving (Show)
data C = C deriving (Show)
data D = D deriving (Show)

data Task a where
  TaskA :: Task A
  TaskB :: Task A -> Task B
  TaskC :: Task A -> Task C
  TaskD :: Task B -> Task C -> Task D
  End :: Task A -> Task D -> Task (A, D)

-- Could be generated. Or we could use some typeclass and Proxy a
toNode :: Task a -> String
toNode TaskA = "A"
toNode (TaskB _) = "B"
toNode (TaskC _) = "C"
toNode (TaskD _ _) = "D"
toNode (End _ _) = "End"

workflow :: Monad m => m (Task (A, D))
workflow = do
  a <- taskA
  b <- taskB a
  c <- taskC a
  d <- taskD b c
  pure $ End a d

taskA :: Monad m => m (Task A)
taskA = return TaskA

taskB :: Monad m => Task A -> m (Task B)
taskB ta = return $ TaskB ta

taskC :: Monad m => Task A -> m (Task C)
taskC ta = return $ TaskC ta

taskD :: Monad m => Task B -> Task C -> m (Task D)
taskD tb tc = return $ TaskD tb tc

-- This is weird, you have to call execute recursively on everything
-- but it makes some sense. Dependency injection esque!
execute :: Task a -> IO a
execute TaskA = do
  putStrLn "ExecuteA"
  return A
execute (TaskB ta) = do
  putStrLn "ExecuteB"
  _ <- execute ta
  return B
execute (TaskC ta) = do
  putStrLn "ExecuteC"
  _ <- execute ta
  return C
execute (TaskD ta tc) = do
  putStrLn "ExecuteD"
  _ <- execute ta
  _ <- execute tc
  return D
execute (End ta td) = do
  putStrLn "ExecuteEnd"
  a <- execute ta
  d <- execute td
  return (a, d)

-- we have all the instructions we need. We just need to turn it into a graph *generically*

-- They return *instructions* rather than actual code
-- we need to convert them into actual actions

-- network TaskA = rep TaskA
-- network (TaskB _) = rep1 TaskB
--
-- type family Analyze a :: b where
--
-- analyze :: Task a -> [(String, String)]
-- analyze _ = []
