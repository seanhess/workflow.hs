{-# LANGUAGE ApplicativeDo #-}

module SimpleCustom where

import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer.Strict

-- import Data.Kind
import Data.Proxy

newtype Custom r a = Custom {runCustom :: r -> IO a}
  deriving (Functor)

instance Applicative (Custom r) where
  pure a = Custom (\_ -> pure a)
  mf <*> ma = Custom $ \r -> do
    f <- runCustom mf r
    a <- runCustom ma r
    pure (f a)

instance Monad (Custom r) where
  ma >>= fmb = Custom $ \r -> do
    a <- runCustom ma r
    runCustom (fmb a) r

newtype Network a = Network {runNetwork :: Writer [(String, String)] a}
  deriving (Functor, Applicative, Monad, MonadWriter [(String, String)])

data A = A deriving (Show)
data B = B deriving (Show)
data C = C deriving (Show)
data D = D deriving (Show)
data Dataset = Dataset deriving (Show)

class Node a where
  nodeName :: Proxy a -> [String]

instance Node A where
  nodeName _ = ["A"]

instance Node B where
  nodeName _ = ["B"]

instance Node C where
  nodeName _ = ["C"]

instance Node D where
  nodeName _ = ["D"]

instance Node () where
  nodeName _ = []

instance (Node a, Node b) => Node (a, b) where
  nodeName _ = mconcat [nodeName @a Proxy, nodeName @b Proxy]

workflow :: Flow Dataset m => m ()
workflow = do
  ta <- task0 runA
  tb <- task1 runB ta
  tc <- task1 runC ta
  td <- task2 runD tb tc
  task2 runEnd ta td

task0 :: (Node a, Flow r m) => (r -> IO a) -> m a
task0 t = task (const t) ()

task1 :: (Node a, Node i, Flow r m) => (i -> r -> IO a) -> (i -> m a)
task1 = task

task2 :: (Node a, Node i, Node v, Flow r m) => (i -> v -> r -> IO a) -> i -> v -> m a
task2 t fi fv = task (uncurry t) (fi, fv)

class (Monad m) => Flow r m where
  task :: (Node a, Node i) => (i -> r -> IO a) -> (i -> m a)

instance Flow Dataset Network where
  task :: forall a i. (Node a, Node i) => (i -> Dataset -> IO a) -> (i -> Network a)
  task _ _ = do
    let adp = nodeName @a Proxy
    let idp = nodeName @i Proxy
    tell $ [(ni, na) | na <- adp, ni <- idp]

    -- WARNING: use undefined instead of requiring Flow to return a functor
    -- if necessary we could use defaults instead
    pure $ error "Network should not evaluate values"

-- pure Proxy

instance Flow Dataset (Custom Dataset) where
  -- TODO: exception catching
  -- TODO: caching
  task :: forall a i. (Node a, Node i) => (i -> Dataset -> IO a) -> (i -> Custom Dataset a)
  task t i = Custom $ t i

-- TODO: Task type, instead of IO?
runA :: Dataset -> IO A
runA _ = pure A

runB :: A -> Dataset -> IO B
runB _ _ = pure B

runC :: A -> Dataset -> IO C
runC _ _ = pure C

runD :: B -> C -> Dataset -> IO D
runD _ _ _ = pure D

runEnd :: A -> D -> Dataset -> IO ()
runEnd a d ds = do
  print a
  print d
  print ds
  pure ()

test :: IO ()
test = do
  let network = workflow :: Network ()
      deps = execWriter $ runNetwork network

  print deps

  _ <- runCustom (workflow :: Custom Dataset ()) Dataset
  pure ()
