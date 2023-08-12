module Types where

data SolarAtlas = SolarAtlas

data Wavelengths = Wavelengths
  deriving (Show)

data Position = Position
  deriving (Show)

data Dataset = Dataset

data Final = Final Wavelengths Position

-- CUSTOM DAG TYPE ---
data Calibrate a where
  Begin :: Calibrate SolarAtlas
  CalWavlengths :: SolarAtlas -> Calibrate Wavelengths
  CalPosition :: Calibrate Position
  Finalize :: Wavelengths -> Position -> Calibrate Final
