{-# LANGUAGE ApplicativeDo #-}

module Flow where

import Control.Monad.State.Lazy
import Flow.Store

-- Specialized Implementation of Build Systems a la Carte
-- https://hackage.haskell.org/package/build
newtype Task k v = Task {runTask :: forall f. (MonadFail f) => (k -> f v) -> f v}
type Tasks k v = k -> Task k v
type Build i k v = Tasks k v -> k -> Store i k v -> Store i k v

-- Example ---------------------
-- sprsh1 :: Tasks String Integer
-- sprsh1 "B1" = Just $ Task $ \f -> do
--   a1 <- f "A1"
--   a2 <- f "A2"
--   pure $ a1 + a2
-- sprsh1 "B2" = Just $ Task $ \f -> (* 2) <$> f "B1"
-- sprsh1 _ = Nothing

data Node
  = A
  | B
  deriving (Show, Eq)

-- \| C
-- \| D

data Result
  = String String
  | Integer Int
  | None
  deriving (Show, Eq)

class FromValue r b where
  fromValue :: r -> Maybe b

instance FromValue Result String where
  fromValue (String s) = Just s
  fromValue _ = Nothing

instance FromValue Result Int where
  fromValue (Integer n) = Just n
  fromValue _ = Nothing

network :: Node -> Task Node Result
network A = taskA
network B = taskB

taskA :: Task Node Result
taskA = Task $ \_ -> do
  pure $ String "hello"

taskB :: Task Node Result
taskB = Task $ \f -> do
  (s :: String) <- fetch f A
  pure $ Integer (length s)

-- we need to be able to throw an error somehow, and cancel the rest. Cancelable! Not simple here...
fetch :: forall f k b. (MonadFail f, FromValue Result b) => (k -> f Result) -> k -> f b
fetch f k = do
  r <- f k :: f Result
  case fromValue r of
    Nothing -> fail "Invalid result"
    Just v -> pure v

busy :: forall m k v. (Eq k, MonadFail m) => Tasks k v -> k -> Store () k v -> m (Store () k v)
busy tasks key store = do
  st <- execStateT (asdf key) store
  pure st
 where
  asdf :: k -> StateT (Store () k v) m v
  asdf k = do
    let task = tasks k :: Task k v
    v <- runTask task asdf
    modify (putValue k v)
    return v

test :: IO ()
test = do
  let store = initialize () (const None)
  result <- busy network B store
  print $ getValue B result

-- print $ dependencyGraph (network) B

-- | Show the dependencies of a given node in the computation
dependencies :: MonadFail m => Task k [k] -> m [k]
dependencies task = do
  runTask task (\k -> pure [k])

dependencyGraph :: forall k m. MonadFail m => Tasks k [k] -> k -> m [(k, k)]
dependencyGraph tasks k = do
  let task = tasks k
  d <- dependencies task :: m [k]
  ds <- mconcat <$> mapM (dependencyGraph tasks) d :: m [(k, k)]
  pure $ (fmap (k,) d) <> ds
