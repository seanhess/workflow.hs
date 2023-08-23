{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module JustFunctions where

import Control.Monad.Reader
import Data.Kind
import Types

-- TODO: Replace this with a Task monad that is also limited?
taskAtlas :: Dataset -> IO SolarAtlas
taskAtlas _ = pure SolarAtlas

taskWavelengths :: SolarAtlas -> Dataset -> IO Wavelengths
taskWavelengths = undefined

taskPosition :: Dataset -> IO Position
taskPosition = undefined

task :: (r -> IO a) -> Workflow r a
task f = Workflow $ ReaderT f

newtype Workflow r a = Workflow {runWorkflow :: ReaderT r IO a}
  deriving (Functor, Applicative, Monad, MonadReader r)

workflow :: Workflow Dataset Final'
workflow = do
  atlas <- task taskAtlas
  pos <- task taskPosition
  wl <- task $ taskWavelengths atlas
  pure $ Final' wl pos

------------------------------------

type family Something a :: b

type instance Something (Calibrate SolarAtlas) = String

type instance Something (SolarAtlas -> Calibrate Wavelengths) = Int

----------------------------------
-- Can I model a network in the type system?
-- We need a better model
data Dag inp a where
  Dep0 :: String -> Dag () a
  Dep1 :: String -> (Dag x a) -> Dag a b
  Dep2 :: String -> (Dag x a, Dag y b) -> Dag (a, b) c

-- Yeah this worked!
network :: Dag Final' Final'
network =
  let wavelengths = Dep1 "Wavelengths" solarAtlas :: Dag SolarAtlas Wavelengths
      solarAtlas = Dep0 "SolarAtlas" :: Dag () SolarAtlas
      position = Dep0 "Position" :: Dag () Position
      finalize = Dep2 "Finalize" (wavelengths, position) :: Dag (Wavelengths, Position) Final'
   in Dep1 "End" finalize

-- data Comp inp a b = Dag inp a :> Dag a b

-- but we can't compose two at once, can we.... make it a .... list!
--
data Dag2 (inp :: [Type]) out where
  Start :: Dag2 '[] ()
  Node :: Dag2 inp out
  (:>) :: Dag2 inp a -> Dag2 (a : as) out -> Dag2 inp out

wvls :: Dag2 (SolarAtlas : as) Wavelengths
wvls = Node

slrat :: Dag2 '[] SolarAtlas
slrat = Node

test :: Dag2 '[] Wavelengths
test = slrat :> wvls
