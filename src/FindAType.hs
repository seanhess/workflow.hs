module FindAType where

import Control.Monad.Identity
import Control.Monad.Writer.Strict
import Data.Kind

import Data.Proxy

-- import Data.Proxy
newtype A = A String deriving (Show)
newtype B = B Int deriving (Show)
newtype C = C String deriving (Show)

-- The monad determines the return type
-- class Monad m => Flow m where
--   type Res m :: Type -> Type

newtype Workflow a = Workflow {runWorkflow :: IO a}
  deriving (Functor, Applicative, Monad)
newtype Graph a = Graph {runGraph :: Writer [(Node, Node)] a}
  deriving (Functor, Applicative, Monad, MonadWriter [(Node, Node)])

newtype Node = Node String

-- instance Flow Workflow where
--   type Res Workflow = IO
--
-- instance Flow Graph where
--   type Res Graph = Node

-- TODO: First, how can we make a system that describes a graph? Ignore execution

workflow :: (Task A m, Task B m, Monad m) => m (Result m B)
workflow = do
  a <- runTask @A undefined
  b <- runTask @B a
  pure b

-- This is the same as a functional dependency in the class
type family Result (a :: Type -> Type) :: Type -> Type
type instance Result IO = Identity
type instance Result Network = Proxy

type family Input (f :: Type -> Type) (a :: Type) :: Type
type instance Input f A = f ()
type instance Input f B = f A

type Network = Writer [(Node, Node)]

class Task a m where
  runTask :: Input (Result m) a -> m ((Result m) a)

-- I don't see how this is better...
instance Task A IO where
  runTask _ = pure $ pure $ A "Hello"

instance Task B IO where
  runTask (Identity (A a)) = pure $ pure $ B $ length a

instance Task A Network where
  runTask _ = do
    tell []
    return Proxy

instance Task B Network where
  -- You still have to manully enter the dependencies!
  -- bullshit
  runTask ia = do
    tell []
    return Proxy

-- runTask :: Task a => F a
-- runTask = undefined

-- I don't know where we define the computations, but it's not here
-- we want to just define the graph

-- it requires no dependencies, go ahead and execute!
-- we define NO computations, only relationships
-- these mean nothing
-- everything is defined above!
-- B depends on A, that's all we are

-- -- Typeclass! Is how we go from TYPE -> Function
-- class Fetch a m where
--   type Inp a :: Type
--   runFetch :: m a
--
-- --
-- -- nodeName :: Proxy a -> m String
--
-- instance Monad m => Fetch A m where
--   type Inp A = ()
--   runFetch = flowA
--
-- instance Monad m => Fetch B m where
--   type Inp B = A
--   runFetch = flowB

-- we can do something here to collect it
-- fetch :: forall a m. (Monad m, Fetch a m) => m (m a)
-- fetch = pure $ runFetch @a
--
-- runTask :: (Monad m, Fetch a m) => (Inp a -> m a) -> m (Inp a) -> m a
-- runTask task inp = do
--   i <- inp
--   task i
--
-- flowA :: (Monad m) => m A
-- flowA = do
--   runTask taskA (pure ())
--
-- taskA :: Monad m => () -> m A
-- taskA _ = pure $ A "Hello"
--
-- flowB :: (Monad m) => m B
-- flowB = do
--   fa <- fetch @A
--   -- we are getting back an applicative than can GIVE us an A. We don't have it yet
--   runTask taskB fa
--
-- taskB :: Monad m => A -> m B
-- taskB (A s) = do
--   pure $ B $ length s

-- taskC :: (Monad m) => m (Result C)
-- taskC = do
--   A s <- fetch @A
--   B l <- fetch @B
--   pure $ C $ "The length of " <> s <> " is " <> show l
--
-- newtype Graph a = Graph (Writer [String] a)
--   deriving (Functor, Applicative, Monad)
