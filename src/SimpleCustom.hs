module SimpleCustom where

import Control.Monad.Writer.Strict
import Data.List (permutations)
import Data.Proxy

newtype Custom a = Custom {runCustom :: (IO a)}
  deriving (Functor, Applicative, Monad)

newtype Network a = Network {runNetwork :: Writer [(String, String)] a}
  deriving (Functor, Applicative, Monad, MonadWriter [(String, String)])

-- TODO: custom monad
-- perform tasks
-- fetch dependencies

data A = A deriving (Show)
data B = B deriving (Show)
data C = C deriving (Show)
data D = D deriving (Show)

class Node a where
  nodeName :: Proxy a -> [String]

instance Node A where
  nodeName _ = ["A"]

instance Node B where
  nodeName _ = ["B"]

instance Node C where
  nodeName _ = ["C"]

instance Node D where
  nodeName _ = ["D"]

instance Node () where
  nodeName _ = []

instance (Node a, Node b) => Node (a, b) where
  -- nodeName :: forall a b. Proxy (a, b) -> [String]
  nodeName _ = mconcat $ [nodeName @a Proxy, nodeName @b Proxy]

-- | Just generate a graph!
workflow :: forall m f. (Flow m f) => m (f (A, D))
workflow = do
  a <- task runA (pure () :: f ())
  b <- task runB a
  c <- task runC a
  d <- task runD ((,) <$> b <*> c)
  pure $ (,) <$> a <*> d

class (Monad m, Applicative f) => Flow m f where
  task :: (Node a, Node i) => (i -> IO a) -> (f i -> m (f a))

instance Flow Network Proxy where
  task :: forall a i. (Node a, Node i) => (i -> IO a) -> (Proxy i -> Network (Proxy a))
  task _ _ = do
    let adp = nodeName @a Proxy
    let idp = nodeName @i Proxy
    tell $ [(ni, na) | na <- adp, ni <- idp]
    pure Proxy

-- it's our job to return an "a"

runA :: () -> IO A
runA = undefined

runB :: A -> IO B
runB = undefined

runC :: A -> IO C
runC = undefined

runD :: (B, C) -> IO D
runD = undefined

test :: IO ()
test = do
  let network = workflow :: Network (Proxy (A, D))
      deps = execWriter $ runNetwork network
  print deps
