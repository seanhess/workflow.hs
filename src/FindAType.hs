module FindAType where

import Control.Monad.Writer.Strict
import Data.Proxy

-- TODO: not the real type. Ignore
newtype Flow a = Flow {runFlow :: IO a}
  deriving (Functor, Applicative, Monad)

newtype A = A String
newtype B = B Int
newtype C = C String

-- Typeclass! Is how we go from TYPE -> Function
class Fetch a m where
  fetch :: m a
  nodeName :: Proxy a -> m String

instance Monad m => Fetch A m where
  fetch = taskA
  nodeName _ = pure "A"

instance Monad m => Fetch B m where
  fetch = taskB
  nodeName _ = pure "B"

instance Monad m => Fetch C m where
  fetch = taskC
  nodeName _ = pure "C"

-- do these definitions expect me to have monadIO
-- when do I run the actual tass?
taskA :: (Monad m) => m A
taskA = do
  pure $ A "Hello"

taskB :: (Monad m) => m B
taskB = do
  A s <- fetch @A
  pure $ B $ length s

taskC :: (Monad m) => m C
taskC = do
  A s <- fetch @A
  B l <- fetch @B
  pure $ C $ "The length of " <> s <> " is " <> show l

test :: IO ()
test = do
  C c <- runFlow taskC
  putStrLn c

newtype Graph a = Graph (Writer [String] a)
  deriving (Functor, Applicative, Monad)
