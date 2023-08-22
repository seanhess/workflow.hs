{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module SimpleCustom where

import Control.Exception (SomeException, catch, throwIO)
import Control.Monad.Identity (Identity (..))
import Flow.Node
import Types

-- import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Debug.Trace

-- import Data.Kind
import Data.Proxy

newtype Pipeline r s a = Pipeline {runPipeline :: r -> s -> IO (a, s)}
  deriving (Functor)

instance Applicative (Pipeline r s) where
  pure a = Pipeline (\_ s -> return (a, s))
  mf <*> ma = Pipeline $ \r s -> do
    (f, s2) <- runPipeline mf r s
    (a, s3) <- runPipeline ma r s2
    pure (f a, s3)

instance Monad (Pipeline r s) where
  ma >>= fmb = Pipeline $ \r s -> do
    (a, s2) <- runPipeline ma r s
    runPipeline (fmb a) r s2

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

-- instance (Node a, Node b, Node c) => Node (a, b, c) where
--   nodeName _ = mconcat [nodeName @a Proxy, nodeName @b Proxy, nodeName @c Proxy]

-- is there a cleaner way to do this?
-- perhaps tasks could have inputs?
-- a custom GADT now?

data DAG a where
  MakeA :: DAG A
  MakeB :: A -> DAG B
  MakeC :: A -> DAG C
  MakeD :: B -> C -> DAG D
  MakeF :: A -> D -> DAG Final

workflow :: Flow Dataset m => m Final
workflow = do
  a <- run' MakeA
  b <- run' $ MakeB a
  c <- run' $ MakeC a
  d <- run' $ MakeD b c
  run' $ MakeF a d

run' :: Flow Dataset m => DAG a -> m a
run' MakeA = run (const taskA) ()
run' (MakeB a) = run taskB a
run' (MakeC a) = run taskC a
run' (MakeD b c) = run2 taskD b c
run' (MakeF a d) = run2 taskEnd a d

run0 :: (Node a, Flow r m) => Task r a -> m a
run0 t = run (const t) ()

run1 :: (Node a, Node i, Flow r m) => (i -> Task r a) -> (i -> m a)
run1 = run

run2 :: (Node a, Node i, Node v, Flow r m) => (i -> v -> Task r a) -> i -> v -> m a
run2 t fi fv = run (uncurry t) (fi, fv)

class (Monad m) => Flow r m where
  run :: (Node a, Node i) => (i -> Task r a) -> (i -> m a)

instance Flow Dataset Network where
  run :: forall a i. (Node a, Node i) => (i -> Task Dataset a) -> (i -> Network a)
  run _ _ = do
    let adp = nodeName @a Proxy
    let idp = nodeName @i Proxy
    traceM $ show ("Network", adp, idp)
    tell $ [(ni, na) | na <- adp, ni <- idp]

    -- WARNING: use undefined instead of requiring Flow to return a functor
    -- if necessary we could use defaults instead
    pure $ error "Network should not evaluate values"

instance Flow Dataset (Pipeline Dataset s) where
  -- TODO: exception catching. I want to crash the FLOW, not the entire program. Maybe I should move the handler
  -- TODO: caching, customizable store... Can use state easily. Hashable...
  run :: forall a i. (Node a, Node i) => (i -> Task Dataset a) -> (i -> Pipeline Dataset s a)
  run t i = Pipeline $ \ds s -> do
    putStrLn $ "RUNNING: " <> mconcat (nodeName @a Proxy)
    -- mc <- cached @s Proxy s
    -- case mc of
    --   Just c -> return (c, s)
    --   Nothing -> do
    res <- catch @SomeException (execute ds) handler
    return (res, s)
   where
    execute :: Dataset -> IO a
    execute ds = do
      runTask (t i) ds

    handler ex = do
      putStrLn "CAUGHT!"
      throwIO ex

-- erm. no... this should be in pipeline
-- should probably do very little work in `run`?
-- or in the monad instance? No it's hard to add constraints there

dataset :: Task Dataset Dataset
dataset = Task $ \ds -> pure ds

-- TODO: Task type, instead of IO?
taskA :: Task Dataset A
taskA = pure A

taskB :: A -> Task Dataset B
taskB _ = pure B

taskC :: A -> Task Dataset C
taskC _ = pure C

taskD :: B -> C -> Task Dataset D
taskD _ _ = pure D

taskEnd :: A -> D -> Task Dataset Final
taskEnd a d = do
  ds <- dataset
  pure $ Final a d ds

test :: IO ()
test = do
  let network = workflow :: Network Final
      deps = execWriter $ runNetwork network

  print deps

  let pipe = workflow :: Pipeline Dataset () Final
  res <- runPipeline pipe Dataset ()
  print res
  pure ()
