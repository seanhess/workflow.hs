module FindAType where

import Data.Proxy

-- IDEA: We are looking for a particular type, given a dataset

class Flow a b r where
  -- probably has to work for ALL monads
  flow :: a -> r -> IO b

data Dataset

data Wavelengths = Wavelengths [Int]

data Position = Position

data SolarAtlas = SolarAtlas

-- how do we know which one is which??
instance Flow SolarAtlas Wavelengths Dataset where
  flow :: SolarAtlas -> Dataset -> IO Wavelengths
  flow = loadWavelengths

instance Flow () Position Dataset where
  flow :: () -> Dataset -> IO Position
  flow = undefined

loadWavelengths :: SolarAtlas -> Dataset -> IO Wavelengths
loadWavelengths _ _ = pure $ Wavelengths []

loadPosition :: Dataset -> IO Position
loadPosition _ = pure Position

workflow :: Dataset -> IO (Wavelengths, Position)
workflow ds = do
  wl <- flow SolarAtlas ds
  ps <- flow () ds
  pure (wl, ps)
