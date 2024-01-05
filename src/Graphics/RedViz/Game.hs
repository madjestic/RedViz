module Graphics.RedViz.Game where

import Linear.Affine
import Linear.V2
import Foreign.C.Types

import Graphics.RedViz.Object
import Graphics.RedViz.Widget
import Graphics.RedViz.Camera
import Graphics.RedViz.Uniforms

data Game = Game
  { tick     :: Integer
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
  { tick     = -1
  , mpos     = P (V2 0 0)
  , quitGame = False
  , cameras  = [defaultCam]
  , uniforms = defaultUniforms
  , objs     = []
  , wgts     = []
  }

initSettings :: GameSettings
initSettings = GameSettings
  { resX = 1280
  , resY = 720 }
