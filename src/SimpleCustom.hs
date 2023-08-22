{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module SimpleCustom where

import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Debug.Trace

-- import Data.Kind
import Data.Proxy
import GHC.Generics hiding (C, D)

newtype Pipeline r a = Pipeline {runPipeline :: ReaderT r IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r)

newtype Network a = Network {runNetwork :: Writer [(String, String)] a}
  deriving newtype (Functor, Applicative, Monad, MonadWriter [(String, String)])

newtype Task r a = Task {runTask :: r -> IO a}
  deriving (Functor)

instance Applicative (Task r) where
  pure a = Task (\_ -> pure a)
  mf <*> ma = Task $ \r -> do
    f <- runTask mf r
    a <- runTask ma r
    pure (f a)

instance Monad (Task r) where
  ma >>= fmb = Task $ \r -> do
    a <- runTask ma r
    runTask (fmb a) r

data A = A deriving (Show, Generic, Node)
data B = B deriving (Show, Generic, Node)
data C = C deriving (Show, Generic, Node)
data D = D deriving (Show, Generic, Node)
data Dataset = Dataset deriving (Show, Generic, Node)
data Final = Final A D Dataset deriving (Show, Generic, Node)

instance Node () where
  nodeName _ = []

class Node a where
  nodeName :: Proxy a -> [String]
  default nodeName :: (Generic a, GNode (Rep a)) => Proxy a -> [String]
  nodeName _ = gnodeName (from (undefined :: a))

instance (Node a, Node b) => Node (a, b) where
  nodeName _ = mconcat [nodeName @a Proxy, nodeName @b Proxy]

class GNode f where
  gnodeName :: f p -> [String]

-- instance (Datatype d) => GNode (M1 D d f) where
--   gnodeName m = [datatypeName m]

instance (Datatype d) => GNode (D1 d f) where
  gnodeName m = [datatypeName m]

-- instance (Node a, Node b, Node c) => Node (a, b, c) where
--   nodeName _ = mconcat [nodeName @a Proxy, nodeName @b Proxy, nodeName @c Proxy]

workflow :: Flow Dataset m => m Final
workflow = do
  ta <- task0 runA
  tb <- task1 runB ta
  tc <- task1 runC ta
  td <- task2 runD tb tc
  task2 runEnd ta td

task0 :: (Node a, Flow r m) => Task r a -> m a
task0 t = task (const t) ()

task1 :: (Node a, Node i, Flow r m) => (i -> Task r a) -> (i -> m a)
task1 = task

task2 :: (Node a, Node i, Node v, Flow r m) => (i -> v -> Task r a) -> i -> v -> m a
task2 t fi fv = task (uncurry t) (fi, fv)

class (Monad m) => Flow r m where
  task :: (Node a, Node i) => (i -> Task r a) -> (i -> m a)

instance Flow Dataset Network where
  task :: forall a i. (Node a, Node i) => (i -> Task Dataset a) -> (i -> Network a)
  task _ _ = do
    let adp = nodeName @a Proxy
    let idp = nodeName @i Proxy
    traceM $ show ("Network", adp, idp)
    tell $ [(ni, na) | na <- adp, ni <- idp]

    -- WARNING: use undefined instead of requiring Flow to return a functor
    -- if necessary we could use defaults instead
    pure $ error "Network should not evaluate values"

-- pure Proxy

instance Flow Dataset (Pipeline Dataset) where
  -- TODO: exception catching
  -- TODO: caching
  task :: forall a i. (Node a, Node i) => (i -> Task Dataset a) -> (i -> Pipeline Dataset a)
  task t i = Pipeline $ ReaderT $ \ds -> do
    runTask (t i) ds

dataset :: Task Dataset Dataset
dataset = Task $ \ds -> pure ds

-- TODO: Task type, instead of IO?
runA :: Task Dataset A
runA = pure A

runB :: A -> Task Dataset B
runB _ = pure B

runC :: A -> Task Dataset C
runC _ = pure C

runD :: B -> C -> Task Dataset D
runD _ _ = pure D

runEnd :: A -> D -> Task Dataset Final
runEnd a d = do
  ds <- dataset
  pure $ Final a d ds

test :: IO ()
test = do
  let network = workflow :: Network Final
      deps = execWriter $ runNetwork network

  print deps

  let pipe = workflow :: Pipeline Dataset Final
  res <- runReaderT (runPipeline pipe) Dataset
  print res
  pure ()
