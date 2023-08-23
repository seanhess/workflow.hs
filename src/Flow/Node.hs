{-# LANGUAGE DefaultSignatures #-}

module Flow.Node where

import Data.Proxy
import GHC.Generics

instance Node () where
  nodeName _ = []

class Node a where
  nodeName :: Proxy a -> [String]
  default nodeName :: (Generic a, GNode (Rep a)) => Proxy a -> [String]
  nodeName _ = gnodeName (from (undefined :: a))

instance (Node a, Node b) => Node (a, b) where
  nodeName _ = mconcat [nodeName @a Proxy, nodeName @b Proxy]

instance (Node a, Node b, Node c) => Node (a, b, c) where
  nodeName _ = mconcat [nodeName @a Proxy, nodeName @b Proxy, nodeName @c Proxy]

class GNode f where
  gnodeName :: f p -> [String]

instance (Datatype d) => GNode (D1 d f) where
  gnodeName m = [datatypeName m]
