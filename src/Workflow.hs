{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Workflow where

-- import Data.Data (Data)

data Workflow r a where
  Workflow :: (r -> IO a) -> Workflow r a

runFlow :: Workflow r a -> (r -> IO a)
runFlow (Workflow f) = f

instance Functor (Workflow r) where
  fmap :: (a -> b) -> Workflow r a -> Workflow r b
  fmap f (Workflow rma) = Workflow $ \r -> do
    a <- rma r
    pure (f a)

instance Applicative (Workflow r) where
  pure a = Workflow $ \_ -> pure a
  fab <*> fa = Workflow $ \r -> do
    ab <- runFlow fab r
    a <- runFlow fa r
    pure (ab a)

instance Monad (Workflow r) where
  (>>=) :: Workflow r a -> (a -> Workflow r b) -> Workflow r b
  fa >>= afb = Workflow $ \r -> do
    a <- runFlow fa r
    runFlow (afb a) r

data SolarAtlas = SolarAtlas

data Wavelengths = Wavelengths
  deriving (Show)

data Position = Position
  deriving (Show)

data Dataset = Dataset

data Final = Final

-- My Custom DAG type --
data Calibrate a where
  Begin :: Calibrate SolarAtlas
  CalWavlengths :: SolarAtlas -> Calibrate Wavelengths
  CalPosition :: Calibrate Position
  Finalize :: Wavelengths -> Position -> Calibrate Final

-- This interpreter is where we run our logic
-- This function must exist
-- TODO: Create a Task, rather than IO
runCal :: Calibrate a -> Dataset -> IO a
runCal Begin _ = pure SolarAtlas
runCal (CalWavlengths _) _ = pure Wavelengths
runCal CalPosition _ = pure Position
runCal (Finalize _ _) _ = pure Final

runCalWorkflow :: Calibrate a -> Workflow Dataset a
runCalWorkflow c = Workflow (runCal c)

-- Step is specialized to my function, can I make it a class?
-- step :: Calibrate a -> Workflow Dataset a
-- step c = Workflow $ \r -> runCal c r

class Monad m => Flow dag m where
  step :: dag a -> m a

-- I have to implement it FOR MY TYPE here
-- which prevents me from doing anything specific to workflow
instance Flow Calibrate (Workflow Dataset) where
  step c = Workflow $ \r -> runCal c r

-- we can't drop to workflow here...
task :: (Show a, Flow dag m) => dag a -> m a
task d = do
  step d

-- this is a member of workflow
storeResult :: Show a => a -> Workflow Dataset ()
storeResult a = Workflow $ \_ -> print a

-- This was the definition of step, I believe
-- step :: (RunTask m, Show a) => dag a -> m a
-- step s = toFlow $ \r -> do
--   a <- runFlow s r
--   print a
--   pure a

-- this works with ANY flow?
-- why is that any better?
-- it lets me specify sily things
-- don't need it. Specialize!
-- figure out another way to draw the graph
workflow :: Flow Calibrate m => m Final
workflow = do
  sa <- step Begin
  wl <- step (CalWavlengths sa)
  p <- step CalPosition
  step $ Finalize wl p
