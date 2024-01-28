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

import Graphics.RedViz.Entity
import Graphics.RedViz.Widget
import Graphics.RedViz.Uniforms

data Game = Game
  { tick :: Double
  , mpos :: Point V2 CInt
  , quit :: Bool
  , cams :: [Camera]
  , unis :: Uniforms
  , objs :: [Object]
  , wgts :: [Widget]
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
  , quit = False
  , cams  = [defaultCam]
  , unis = defaultUniforms
  , objs     = []
  , wgts     = []
  }

initSettings :: GameSettings
initSettings =  GameSettings
  { resX = 1280
  , resY = 720 }
