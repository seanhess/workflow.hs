{-# LANGUAGE ApplicativeDo #-}

module Build where

import Control.Applicative (Const (..))
import Control.Monad.State.Strict as State
import Data.Default (Default, def)
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- Existential restriction on the RHS here
-- The external type doesn't depend on it, but internally it can use any applicative
newtype Task k v = Task {runTask :: forall f. Applicative f => (k -> f v) -> f v}
type Tasks k v = k -> Maybe (Task k v)

data MyTasks = A1 | A2 | B1 | B2
  deriving (Show, Eq, Ord)

-- this is equivalent to the *implementation* from the previous one
-- isomorphic to the applicative function?
-- TODO: does this work with different return types?
sprsh1 :: Tasks MyTasks Integer
sprsh1 B1 = Just $ Task $ \fetch -> do
  -- Note, that this function has no idea what applicative we are running in
  a1 <- fetch A1
  a2 <- fetch A2
  pure $ a1 + a2
sprsh1 B2 = Just $ Task $ \fetch -> do
  -- ApplicativeDo!
  b1 <- fetch B1
  return $ b1 * 2
sprsh1 _ = Nothing

-- But i'm thinkin fo them in terms of tasks, rather than
taskA1 :: Task MyTasks String
taskA1 = Task $ \_ -> pure "Hello"

taskA2 :: Task MyTasks Integer
taskA2 = Task $ \_ -> pure 3

-- Oh, no, it doesn't work!
-- it expects the result of previous steps to always be the same type! Yikes!
-- taskB1 :: Task MyTasks String
-- taskB1 = Task $ \fetch -> do
--   msg <- fetch A1
--   n <- fetch A2
--   pure $ replicate n msg

workflow :: forall m. Applicative m => m Integer
workflow = do
  let a1 = calcA1
  let a2 = calcA2
  let b1 = calcB1 a1 a2
  let b2 = calcB2 b1
  -- now the computation DOES depend on a1, so it can't be done in applicative!
  -- b2 <- calcB2 a1
  -- pure $ a1 * 2
  b2
 where
  -- workflowDo :: Applicative m => m Integer
  -- workflowDo = do
  --   a1 <- calcA1
  --   a2 <- calcA2
  --   -- You need to pass the other applicatives INTO the other tasks in order for it to make any sense
  --   b1 <- calcB1 a1 a2
  --   b2 <- calcB2 b1
  --   pure b2

  calcA1 :: Applicative m => m Integer
  calcA1 = pure 10

  calcA2 :: Applicative m => m Integer
  calcA2 = pure 20

  -- everything takes applicatives!
  calcB1 :: Applicative m => m Integer -> m Integer -> m Integer
  calcB1 ca1 ca2 = do
    a1 <- ca1
    a2 <- ca2
    pure $ a1 + a2

  calcB2 :: Applicative m => m Integer -> m Integer
  calcB2 b1 = (* 2) <$> b1

type Build i k v = Tasks k v -> k -> Map k v -> Map k v

-- We are running in
busy :: forall k v. (Ord k, Default v) => (k -> Maybe (Task k v)) -> k -> Map k v -> Map k v
busy tasks key store = execState (fetch key) store
 where
  -- Hmm... it's more permissive here
  fetch :: MonadState (Map k v) m => k -> m v
  fetch k = case tasks k of
    Nothing -> do
      gets (fromMaybe def <$> Map.lookup k)
    Just task -> do
      v <- runTask task fetch
      modify (Map.insert k v)
      return v

test :: IO ()
test = do
  let store = Map.fromList [(A1, 10), (A2, 20)]
      res = busy sprsh1 B2 store
  print res
  print $ Map.lookup B2 res

-- | Show the dependencies of a given node in the computation
dependencies :: Task k v -> [k]
dependencies task = getConst $ runTask task (\k -> Const [k])

dependencyGraph :: Tasks k v -> k -> [(k, k)]
dependencyGraph tasks k =
  case tasks k of
    Nothing -> []
    Just task ->
      let deps = dependencies task
       in fmap (k,) deps <> mconcat (fmap (dependencyGraph tasks) deps)
