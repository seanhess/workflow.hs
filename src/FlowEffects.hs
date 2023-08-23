{-# LANGUAGE LambdaCase #-}

module FlowEffects where

import Data.Proxy
import Effectful
import Effectful.Dispatch.Dynamic

-- import Effectful.Reader.Dynamic
import Effectful.Writer.Dynamic
import Flow.Node
import Types

newtype Task inp a = Task {runTask :: inp -> IO a}

data Flow :: Effect where
  RunTask :: (Node a, Node inp) => Task inp a -> inp -> Flow m a
  AskDataset :: Flow m Dataset

type instance DispatchOf Flow = 'Dynamic

dataset :: Flow :> es => Eff es Dataset
dataset = send AskDataset

-- TODO: I want workflow to look like this! Really easy to see the network
workflow :: (Flow :> es) => Eff es Final
workflow = do
  a <- stepA
  b <- stepB a
  c <- stepC a
  d <- stepD b c
  stepEnd a d

stepA :: (Flow :> es) => Eff es A
stepA = run (const execA) ()
 where
  execA :: IO A
  execA = pure A

stepB :: (Flow :> es) => A -> Eff es B
stepB = run execB
 where
  execB :: A -> IO B
  execB _ = pure B

stepC :: (Flow :> es) => A -> Eff es C
stepC = run execC
 where
  execC :: A -> IO C
  execC _ = pure C

stepD :: (Flow :> es) => B -> C -> Eff es D
stepD b c = run (uncurry execD) (b, c)
 where
  execD :: B -> C -> IO D
  execD _ _ = pure D

stepEnd :: (Flow :> es) => A -> D -> Eff es Final
stepEnd a' d' = do
  ds <- dataset
  run (\(a, d) -> execEnd a d ds) (a', d')
 where
  execEnd :: A -> D -> Dataset -> IO Final
  execEnd a d ds = do
    pure $ Final a d ds

run :: (Node a, Node inp, (Flow :> es)) => (inp -> IO a) -> inp -> Eff es a
run ft i = send $ RunTask (Task ft) i

runFlowIO ::
  (IOE :> es) =>
  Dataset ->
  Eff (Flow : es) a ->
  Eff es a
runFlowIO ds = interpret $ \_ -> \case
  (RunTask (Task ft) i) -> liftIO $ ft i
  AskDataset -> pure ds

runFlowNetwork ::
  forall es a.
  Dataset ->
  (IOE :> es) =>
  Eff (Flow : es) a ->
  Eff es DAG
runFlowNetwork ds = reinterpret (execWriterLocal @DAG) $ \_ -> \case
  (RunTask t _) -> runTaskNetwork t
  AskDataset -> pure ds

runTaskNetwork :: forall a inp es. (Node a, Node inp, Writer DAG :> es) => Task inp a -> Eff es a
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
  putStrLn "RUN FLOW IO"
  print f

  d <- runEff $ runFlowNetwork Dataset workflow
  putStrLn "\nRUN FLOW NETWORK"
  print d
