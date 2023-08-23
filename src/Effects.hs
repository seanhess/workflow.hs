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
