--------------------------------------------------------------------------------
-- |
-- Module      :  Camera
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic camera structure.
--
--------------------------------------------------------------------------------

module Graphics.RedViz.Camera where

import Data.UUID

import Graphics.RedViz.Solvable
import Graphics.RedViz.Transformable
import Linear.V3

data Camera
  =  Camera
     { name       :: String
     , apt        :: Double
     , foc        :: Double
     , ctransform :: Transformable
     , mouseS     :: V3 Double -- mouse    "sensitivity"
     , keyboardRS :: V3 Double -- keyboard "rotation sensitivity"
     , keyboardTS :: V3 Double -- keyboard "translation sensitivity"
     , cslvrs     :: [Solvable]
     , uuid       :: UUID
     , parent     :: UUID
     } deriving Show

defaultCam :: Camera
defaultCam =
  Camera
  {
    name       = "PlayerCamera"
  , apt        = 50.0
  , foc        = 100.0
  , ctransform = defaultCamTransformable { tslvrs = [defaultCamSolver]}
  , mouseS     = -0.0025
  , keyboardRS = 0.05
  , keyboardTS = 0.05
  , cslvrs     = []
  , uuid       = nil
  , parent     = nil
  }

defaultCamSolver :: Solvable
defaultCamSolver =
  Controller
  { cvel   = (V3 0 0 0) -- velocity
  , cypr   = (V3 0 0 0) -- rotation
  , cyprS  = (V3 0 0 0) -- sum of rotations
  }
