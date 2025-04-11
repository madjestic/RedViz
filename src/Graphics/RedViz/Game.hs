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
import Data.Binary as DB
import Data.ByteString.Lazy as DBL (writeFile, readFile)
import Data.Functor

import Graphics.RedViz.Entity
import Graphics.RedViz.Widget
import Graphics.RedViz.Uniforms
       
instance Binary Game where
  put (Game t m q c u o w) = do
    put t
    put m
    put q
    -- put me
    -- put p
    put c 
    put u
    put o
    put w
  get = do 
    t  <- get 
    m  <- get 
    q  <- get 
    c  <- get 
    u  <- get 
    o  <- get 
    w  <- get
    return $ Game t m q c u o w

data Game = Game
  { tick  :: Integer
  , mpos  :: Point V2 Int
  , quit  :: Bool
  , cams  :: [Camera]
  , unis  :: Uniforms
  , objs  :: [Object]
  , wgts  :: [Widget]
  } deriving Show

saveGame :: FilePath -> Game -> IO ()
saveGame fp game0 = do
  DBL.writeFile fp (encode game0)

loadGame :: FilePath -> IO Game
loadGame fp = do
  --DBL.readFile fp >>= return . decode
  DBL.readFile fp <&> decode

data GameSettings = GameSettings
  { resX :: Int 
  , resY :: Int 
  } deriving Show

initGame :: Game
initGame =
  Game
  { tick  = 0
  , mpos  = P (V2 0 0)
  , quit  = False
  , cams  = [defaultEntity]
  , unis  = defaultUniforms
  , objs  = []
  , wgts  = []
  }

initSettings :: GameSettings
initSettings =  GameSettings
  { resX = 1280
  , resY = 720 }

parentabless :: Game -> [Entity]
parentabless g0 = es
  where
    es = filter isParentable $ objs g0 -- ++ cams g0
      where
        isParentable e =
          case parentables e of
            [] -> False
            _  -> True

controllabless :: Game -> [Entity]
controllabless g0 = es
  where
    es = filter isControllable $ cams g0 -- ++ objs g0
      where
        isControllable e =
          case controllables e of
            [] -> False
            _  -> True
