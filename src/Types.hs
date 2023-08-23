{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Flow.Node
import GHC.Generics (Generic)

data SolarAtlas = SolarAtlas

data Wavelengths = Wavelengths
  deriving (Show)

data Position = Position
  deriving (Show)

-- CUSTOM DAG TYPE ---
data Calibrate a where
  Begin :: Calibrate SolarAtlas
  CalWavlengths :: SolarAtlas -> Calibrate Wavelengths
  CalPosition :: Calibrate Position
  Finalize :: Wavelengths -> Position -> Calibrate Final'

data Final' = Final' Wavelengths Position

data A = A deriving (Show, Generic, Node)
data B = B deriving (Show, Generic, Node)
data C = C deriving (Show, Generic, Node)
data D = D deriving (Show, Generic, Node)
data Dataset = Dataset deriving (Show, Generic, Node)
data Final = Final A D Dataset deriving (Show, Generic, Node)
