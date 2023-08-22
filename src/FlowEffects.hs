module FlowEffects where

import Data.Proxy
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Writer.Dynamic
import Flow.Node
import Types

type Task r a = r -> IO a

data Flow r :: Effect where
  -- the ONLY thing it can do is run tasks
  -- that IS easy to write!
  RunTask :: (Node a, Node i) => (i -> Task r a) -> i -> Flow r m a

type instance DispatchOf (Flow r) = 'Dynamic

workflow :: (Flow Dataset :> es) => Eff es Final
workflow = do
  a <- run0 taskA
  b <- run1 taskB a
  c <- run1 taskC a
  d <- run2 taskD b c
  run2 taskEnd a d

run f i = send $ RunTask f i
run :: (Node a, Node i, Flow r :> es) => (i -> Task r a) -> i -> Eff es a
run0 :: (Node a, Flow r :> es) => Task r a -> Eff es a
run0 t = run (const t) ()

run1 :: (Node a, Node i, Flow r :> es) => (i -> Task r a) -> i -> Eff es a
run1 = run

run2 :: (Node a, Node i, Node v, Flow r :> es) => (i -> v -> Task r a) -> i -> v -> Eff es a
run2 t i v = run (uncurry t) (i, v)

taskA :: Task Dataset A
taskA _ = pure A

taskB :: A -> Task Dataset B
taskB _ _ = pure B

taskC :: A -> Task Dataset C
taskC _ _ = pure C

taskD :: B -> C -> Task Dataset D
taskD _ _ _ = pure D

taskEnd :: A -> D -> Task Dataset Final
taskEnd a d ds = do
  pure $ Final a d ds

runFlowIO ::
  (IOE :> es) =>
  r ->
  Eff (Flow r : es) a ->
  Eff es a
runFlowIO r = interpret $ \_ (RunTask t i) -> do
  liftIO $ t i r

runFlowNetwork ::
  forall r es a.
  (IOE :> es) =>
  Eff (Flow r : es) a ->
  Eff es DAG
runFlowNetwork = reinterpret (execWriterLocal @DAG) $ \_ (RunTask t i) -> do
  runTask t i
 where
  runTask :: forall a i r es. (Node a, Node i, Writer DAG :> es) => (i -> Task r a) -> i -> Eff es a
  runTask _ _ = do
    let adp = nodeName @a Proxy
    let idp = nodeName @i Proxy
    tell $ [(ni, na) | na <- adp, ni <- idp]

    -- WARNING: use error instead of requiring Flow to return a functor
    -- if necessary we could return defaults instead, but they are
    -- never used
    pure $ error "Network should not evaluate values"

type DAG = [(String, String)]

test :: IO ()
test = do
  f <- runEff $ runFlowIO Dataset workflow
  print ("FINAL", f)

  d <- runEff $ runFlowNetwork @Dataset workflow
  putStrLn "NETWORK"
  print d
