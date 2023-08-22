module TaglessFinal where

data A = A deriving (Show)
data B = B deriving (Show)
data C = C deriving (Show)
data D = D deriving (Show)

class Monad m => Workflow m where
  taskA :: m A
  taskB :: A -> m B
  taskC :: A -> m C
  taskD :: B -> C -> m D

workflow :: Workflow m => m (A, D)
workflow = do
  a <- taskA
  b <- taskB a
  c <- taskC a
  d <- taskD b c
  pure (a, d)

instance Workflow IO where
  taskA = do
    putStrLn "Executing A"
    return A

  taskB a = do
    putStrLn $ "Executing B: " <> show a
    return B

  taskC a = do
    putStrLn $ "Executing C: " <> show a
    return C

  taskD b c = do
    putStrLn $ "Executing D: " <> show b <> " " <> show c
    return D

newtype Node = Node String
type Graph = [(Node, Node)]

--
data Network a where
  Network :: Graph -> Network a

instance Functor Network where
  fmap :: forall a b. (a -> b) -> Network a -> Network b
  fmap _ (Network g) = Network g

instance Applicative Network where
  pure :: a -> Network a
  pure _ = Network mempty
  Network g <*> Network g2 = Network (g <> g2)

--
-- -- we are given a function that expects an a! We can't produce an A
-- -- can we use default instances?
instance Monad Network where
  (>>=) :: forall a b. Network a -> (a -> Network b) -> Network b
  Network _ >>= _ = undefined

-- we know 'a' will never be read
-- for it to be automatic, it has to happen here
-- let Network gb = fnb undefined
--     fromNode = Node (undefined :: a)
--     toNode = Node (undefined :: b)
--  in Network $ (fromNode, toNode) : ga <> gb

-- in my type we don't actually carry A and B around?
-- I can't enforce it!
instance Workflow Network where
  taskA = pure undefined
  taskB _ = pure undefined
  taskC _ = pure undefined
  taskD _ _ = pure undefined
