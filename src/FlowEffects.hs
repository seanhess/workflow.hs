{-# LANGUAGE LambdaCase #-}

module FlowEffects where

import Data.Proxy
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Effectful.Writer.Dynamic
import Flow.Node
import Types

newtype Task inp a = Task {runTask :: inp -> IO a}

data Flow :: Effect where
  RunTask :: (Node a, Input inp) => Task inp a -> inp -> Flow m a

type instance DispatchOf Flow = 'Dynamic

-- easy to make an alias for my flow
type MyFlow es = (Reader Dataset :> es, Flow :> es)

dataset :: MyFlow es => Eff es Dataset
dataset = ask

-- TODO: I want workflow to look like this! Really easy to see the network
workflow :: MyFlow es => Eff es Final
workflow = do
  a <- stepA
  b <- stepB a
  c <- stepC a
  d <- stepD b c
  stepEnd a d

stepA :: (Flow :> es) => Eff es A
stepA = runIO (const execA) ()
 where
  execA :: IO A
  execA = pure A

stepB :: (Flow :> es) => A -> Eff es B
stepB = runIO execB
 where
  execB :: A -> IO B
  execB _ = pure B

stepC :: (Flow :> es) => A -> Eff es C
stepC = runIO execC
 where
  execC :: A -> IO C
  execC _ = pure C

stepD :: (Flow :> es) => B -> C -> Eff es D
stepD b c = runIO (uncurry execD) (b, c)
 where
  execD :: B -> C -> IO D
  execD _ _ = pure D

stepEnd :: (Reader Dataset :> es, Flow :> es) => A -> D -> Eff es Final
stepEnd a' d' = do
  ds <- dataset
  runIO (\(a, d) -> execEnd a d ds) (a', d')
 where
  execEnd :: A -> D -> Dataset -> IO Final
  execEnd a d ds = do
    pure $ Final a d ds

runIO :: (Node a, Input inp, (Flow :> es)) => (inp -> IO a) -> inp -> Eff es a
runIO ft i = send $ RunTask (Task ft) i

runFlowIO ::
  (IOE :> es) =>
  Eff (Flow : es) a ->
  Eff es a
runFlowIO = interpret $ \_ -> \case
  (RunTask (Task ft) i) -> liftIO $ ft i

runFlowNetwork ::
  forall es a.
  (IOE :> es) =>
  Eff (Flow : es) a ->
  Eff es DAG
runFlowNetwork = reinterpret (execWriterLocal @DAG) $ \_ -> \case
  (RunTask t _) -> runTaskNetwork t

runTaskNetwork :: forall a inp es. (Node a, Input inp, Writer DAG :> es) => Task inp a -> Eff es a
runTaskNetwork _ = do
  let an = nodeName @a Proxy
  let ins = nodeNames @inp Proxy
  tell $ [(i, an) | i <- ins]

  -- WARNING: use error instead of requiring Flow to return a functor
  -- if necessary we could return defaults instead, but they are
  -- never used
  pure $ error "Network should not evaluate values"

type DAG = [(String, String)]

test :: IO ()
test = do
  f <- runEff $ runReader (Dataset "hello") $ runFlowIO workflow
  putStrLn "RUN FLOW IO!"
  print f

  d <- runEff $ runReader (Dataset "net") $ runFlowNetwork workflow
  putStrLn "\nRUN FLOW NETWORK"
  print d
