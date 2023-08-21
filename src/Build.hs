module Build where

import Control.Applicative (Const (..), (<|>))
import Control.Monad.State.Strict as State
import Data.Default (Default, def)
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- Existential restriction on the RHS here...
newtype Task k v = Task {runTask :: forall f. Applicative f => (k -> f v) -> f v}
type Tasks k v = k -> Maybe (Task k v)

-- this is equivalent to my monadic definition
sprsh1 :: Tasks MyTasks Integer
sprsh1 B1 = Just $ Task $ \fetch -> (+) <$> fetch A1 <*> fetch A2
sprsh1 B2 = Just $ Task $ \fetch -> (* 2) <$> fetch B1
sprsh1 _ = Nothing

type Build i k v = Tasks k v -> k -> Map k v -> Map k v

--
-- My version cannot be run in Monad
busy :: forall k v. (Ord k, Default v) => (k -> Maybe (Task k v)) -> k -> Map k v -> Map k v
busy tasks key store = execState (fetch key) store
 where
  fetch :: k -> State (Map k v) v
  fetch k = case tasks k of
    Nothing -> do
      res <- gets (fromMaybe def <$> Map.lookup k)
      pure res
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

data MyTasks = A1 | A2 | B1 | B2
  deriving (Show, Eq, Ord)

dependencyGraph :: Tasks k v -> k -> [(k, k)]
dependencyGraph tasks k =
  case tasks k of
    Nothing -> []
    Just task ->
      let deps = dependencies task
       in fmap (k,) deps <> mconcat (fmap (dependencyGraph tasks) deps)
