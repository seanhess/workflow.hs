{-# LANGUAGE LambdaCase #-}

module Effects where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = 'Dynamic

runFileSystemIO ::
  (IOE :> es) =>
  Eff (FileSystem : es) a ->
  Eff es a
runFileSystemIO = interpret $ \_ -> \case
  ReadFile fp -> liftIO $ readFile fp
  WriteFile fp cnt -> liftIO $ writeFile fp cnt

data Debug :: Effect where
  Debug :: Show a => a -> Debug m ()

runDebugIO ::
  (IOE :> es) =>
  Eff (Debug : es) a ->
  Eff es a
runDebugIO = interpret $ \_ -> \case
  Debug a -> liftIO $ print a

type instance DispatchOf Debug = 'Dynamic

doStuff :: (FileSystem :> es, Debug :> es) => Eff es ()
doStuff = do
  let fp = "./test.txt"
  send (WriteFile fp "HELLO")
  c <- send (ReadFile fp)
  send (Debug ("WOOT", c))
  pure ()

test :: IO ()
test = do
  runEff . runDebugIO . runFileSystemIO $ doStuff


runFlowNetwork ::
  (IOE :> es) =>
  r ->
  Eff (Flow r : es) a ->
  Eff es a
runFlowNetwork _ = interpret $ \_ (RunTask t i) -> do
  runTask t i
 where
  runTask :: forall a i r es. (Node a, Node i, Writer DAG :> es) => (i -> Task r a) -> i -> Eff es a
  runTask _ _ = do
    let adp = nodeName @a Proxy
    let idp = nodeName @i Proxy
    tell $ [(ni, na) | na <- adp, ni <- idp]

    -- WARNING: use undefined instead of requiring Flow to return a functor
    -- if necessary we could use defaults instead
    pure $ error "Network should not evaluate values"

