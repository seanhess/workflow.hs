module FindAType where

import Types

-- IDEA: We are looking for a particular type, given a dataset
-- we are producing a bunch of resources
-- the last one who needs it gets it!

class Flow a b r where
  -- probably has to work for ALL monads
  flow :: a -> r -> IO b

-- how do we know which one is which??
instance Flow SolarAtlas Wavelengths Dataset where
  flow :: SolarAtlas -> Dataset -> IO Wavelengths
  flow = loadWavelengths

instance Flow () Position Dataset where
  flow :: () -> Dataset -> IO Position
  flow = undefined

loadWavelengths :: SolarAtlas -> Dataset -> IO Wavelengths
loadWavelengths _ _ = pure Wavelengths

loadPosition :: Dataset -> IO Position
loadPosition _ = pure Position

workflow :: Dataset -> IO (Wavelengths, Position)
workflow ds = do
  wl <- flow SolarAtlas ds
  ps <- flow () ds
  pure (wl, ps)
