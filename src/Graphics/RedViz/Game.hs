--------------------------------------------------------------------------------
-- |
-- Module      :  Game
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic camera structure.
--
--------------------------------------------------------------------------------
module Graphics.RedViz.Game where

import Linear.Affine
import Linear.V2
import Foreign.C.Types

import Graphics.RedViz.Object
import Graphics.RedViz.Widget
import Graphics.RedViz.Camera
import Graphics.RedViz.Uniforms

data Game = Game
  { tick     :: Double
  , mpos     :: Point V2 CInt
  , quitGame :: Bool
  , cameras  :: [Camera]
  , uniforms :: Uniforms
  , objs     :: [Object]
  , wgts     :: [Widget]
  } deriving Show

data GameSettings = GameSettings
  { resX :: Int 
  , resY :: Int 
  } deriving Show

initGame :: Game
initGame =
  Game
  { tick     = 0.0
  , mpos     = P (V2 0 0)
  , quitGame = False
  , cameras  = [defaultCam]
  , uniforms = defaultUniforms
  , objs     = []
  , wgts     = []
  }

initSettings :: GameSettings
initSettings =  GameSettings
  { resX = 1280
  , resY = 720 }
