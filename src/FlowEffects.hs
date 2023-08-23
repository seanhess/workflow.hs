module FlowEffects where

import Data.Proxy
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Writer.Dynamic
import Flow.Node
import Types

newtype Task r inp a = Task {runTask :: r -> IO a}

data Flow r :: Effect where
  -- the ONLY thing it can do is run tasks
  -- that IS easy to write!
  RunTask :: (Node a, Node inp) => Task r inp a -> Flow r m a

type instance DispatchOf (Flow r) = 'Dynamic

workflow :: (Flow Dataset :> es) => Eff es Final
workflow = do
  a <- run $ task (const execA) ()
  b <- run $ task execB a
  c <- run $ task execC a
  d <- run $ task (uncurry execD) (b, c)
  run $ task (uncurry execEnd) (a, d)

run :: (Node a, Node inp, Flow r :> es) => Task r inp a -> Eff es a
run t = send $ RunTask t

task :: (Node a, Node inp) => (inp -> r -> IO a) -> inp -> Task r inp a
task exec i = Task (exec i)

-- run0 :: (Node a, Flow r :> es) => Task r a -> Eff es a
-- run0 t = run (const t) ()
--
-- run1 :: (Node a, Node i, Flow r :> es) => (i -> Task r a) -> i -> Eff es a
-- run1 = run
--
-- run2 :: (Node a, Node i, Node v, Flow r :> es) => (i -> v -> Task r a) -> i -> v -> Eff es a
-- run2 t i v = run (uncurry t) (i, v)

-- these are IO functions, not tasks!
execA :: Dataset -> IO A
execA _ = pure A

execB :: A -> Dataset -> IO B
execB _ _ = pure B

execC :: A -> Dataset -> IO C
execC _ _ = pure C

execD :: B -> C -> Dataset -> IO D
execD _ _ _ = pure D

execEnd :: A -> D -> Dataset -> IO Final
execEnd a d ds = do
  pure $ Final a d ds

runFlowIO ::
  (IOE :> es) =>
  r ->
  Eff (Flow r : es) a ->
  Eff es a
runFlowIO r = interpret $ \_ (RunTask (Task t)) -> do
  liftIO $ t r

runFlowNetwork ::
  forall r es a.
  (IOE :> es) =>
  Eff (Flow r : es) a ->
  Eff es DAG
runFlowNetwork = reinterpret (execWriterLocal @DAG) $ \_ (RunTask t) -> do
  runTaskNetwork t

runTaskNetwork :: forall a inp r es. (Node a, Node inp, Writer DAG :> es) => (Task r inp a) -> Eff es a
runTaskNetwork _ = do
  let adp = nodeName @a Proxy
  let idp = nodeName @inp Proxy
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
