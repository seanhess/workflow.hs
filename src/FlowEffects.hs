module FlowEffects where

-- import Data.Kind
import Data.Proxy
import Effectful
import Effectful.Dispatch.Dynamic

-- import Effectful.Reader.Dynamic
import Effectful.Writer.Dynamic
import Flow.Node
import Types

-- it depends on previous steps!
-- newtype Task inp a = Task {runTask :: forall es. Reader Dataset :> es => inp -> Eff es a}
newtype Task inp a = Task {runTask :: Dataset -> inp -> IO a}

data Flow :: Effect where
  -- the ONLY thing it can do is run tasks
  -- that IS easy to write!
  RunTask :: (Node a, Node inp) => Task inp a -> inp -> Flow m a

type instance DispatchOf Flow = 'Dynamic

-- TODO: I want workflow to look like this! Really easy to see the network
workflow :: (Flow :> es) => Eff es Final
workflow = do
  a <- stepA
  b <- stepB a
  c <- stepC a
  d <- stepD b c
  stepEnd a d

stepA :: Flow :> es => Eff es A
stepA = run (\_ _ -> execA) ()
 where
  execA :: IO A
  execA = pure A

stepB :: Flow :> es => A -> Eff es B
stepB = run (\_ a -> execB a)
 where
  execB :: A -> IO B
  execB _ = pure B

stepC :: Flow :> es => A -> Eff es C
stepC = run (\_ a -> execC a)
 where
  execC :: A -> IO C
  execC _ = pure C

stepD :: Flow :> es => B -> C -> Eff es D
stepD b c = run (\_ -> uncurry execD) (b, c)
 where
  execD :: B -> C -> IO D
  execD _ _ = pure D

stepEnd :: Flow :> es => A -> D -> Eff es Final
stepEnd = curry $ run (\ds (a, d) -> execEnd a d ds)
 where
  execEnd :: A -> D -> Dataset -> IO Final
  execEnd a d ds = do
    pure $ Final a d ds

run :: (Node a, Node inp, Flow :> es) => (Dataset -> inp -> IO a) -> inp -> Eff es a
run ft i = send $ RunTask (Task ft) i

-- where
--   task :: (Node a, Node inp, ToTask inp) =>  -> inp -> Task inp a
--   task exec i = Task (toTask exec i)

--
-- type family Input a :: Type where
--   Input (r -> IO a) = ()
--   Input (i -> r -> IO a) = i
--   Input (i -> v -> r -> IO a) = (i, v)

-- type family Input a :: Type where
--   Input () = ()
--   Input (i -> r -> IO a) = i
--   Input (i -> v -> r -> IO a) = (i, v)

-- run0 :: (Node a, Flow r :> es) => Task r a -> Eff es a
-- run0 t = run (const t) ()
--
-- run1 :: (Node a, Node i, Flow r :> es) => (i -> Task r a) -> i -> Eff es a
-- run1 = run
--
-- run2 :: (Node a, Node i, Node v, Flow r :> es) => (i -> v -> Task r a) -> i -> v -> Eff es a
-- run2 t i v = run (uncurry t) (i, v)
--
-- task' :: exec -> Task r (Input exec a) a
-- task' = _

runFlowIO ::
  (IOE :> es) =>
  Dataset ->
  Eff (Flow : es) a ->
  Eff es a
runFlowIO ds = interpret $ \_ (RunTask (Task ft) i) -> do
  liftIO $ ft ds i

runFlowNetwork ::
  forall es a.
  (IOE :> es) =>
  Eff (Flow : es) a ->
  Eff es DAG
runFlowNetwork = reinterpret (execWriterLocal @DAG) $ \_ (RunTask t _) -> do
  runTaskNetwork t

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
  print ("FINAL", f)

  d <- runEff $ runFlowNetwork workflow
  putStrLn "NETWORK"
  print d
