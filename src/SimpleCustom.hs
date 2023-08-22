module SimpleCustom where

import Control.Monad.Writer.Strict
import Data.Kind
import Data.Proxy

newtype Custom a = Custom {runCustom :: IO a}
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
  nodeName _ = mconcat [nodeName @a Proxy, nodeName @b Proxy]

-- I don't love this...it's almost good
workflow :: forall m f. (Flow m f) => m (f (A, D))
workflow = do
  a <- task0 runA
  b <- task1 runB a
  c <- task1 runC a
  d <- task2 runD b c
  pure $ _ (a, d)

task0 :: (Node a, Flow m f) => IO a -> m (f a)
task0 t = task (const t) (pure ())

task1 :: (Node a, Node i, Flow m f) => (i -> IO a) -> (f i -> m (f a))
task1 = task

task2 :: (Node a, Node i, Node v, Flow m f) => (i -> v -> IO a) -> f i -> f v -> m (f a)
task2 t fi fv = task (uncurry t) $ (,) <$> fi <*> fv

class (Monad m, Applicative f) => Flow m f where
  task :: (Node a, Node i) => (i -> IO a) -> (f i -> m (f a))

instance Flow Network Proxy where
  task :: forall a i. (Node a, Node i) => (i -> IO a) -> (Proxy i -> Network (Proxy a))
  task _ _ = do
    let adp = nodeName @a Proxy
    let idp = nodeName @i Proxy
    tell $ [(ni, na) | na <- adp, ni <- idp]
    pure Proxy

type family Input a :: Type
type instance Input (IO a) = ()
type instance Input (i -> IO a) = i
type instance Input (i1 -> i2 -> IO a) = (i1, i2)

-- The tasks are sane!
runA :: IO A
runA = undefined

runB :: A -> IO B
runB = undefined

runC :: A -> IO C
runC = undefined

runD :: B -> C -> IO D
runD = undefined

test :: IO ()
test = do
  let network = workflow :: Network (Proxy (A, D))
      deps = execWriter $ runNetwork network
  print deps
