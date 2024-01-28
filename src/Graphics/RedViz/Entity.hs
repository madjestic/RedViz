--------------------------------------------------------------------------------
-- |
-- Module      :  Entity
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

module Graphics.RedViz.Entity where

import Data.UUID
import Linear.V3
import Linear.V4 ( V4(V4) )
import Linear.Matrix

import Graphics.RedViz.Solvable hiding (parent)
import Graphics.RedViz.Transformable
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Backend (BackendOptions, defaultBackendOptions)
import Graphics.RedViz.Material as R
import Graphics.RedViz.Texture hiding (uuid)
import Graphics.Rendering.OpenGL.GL.Texturing

type Object = Entity
type Camera = Entity

data PType = Default
           | Font
           | Icon

instance Show PType where
  show Default = "Default"
  show Font    = "Font"
  show Icon    = "Icon"

data Entity
  =  Entity
     { lable      :: String
     , transform  :: Transformable
     , slvrs      :: [Solvable]
     , uuid       :: UUID
     , parent     :: UUID
     , parented   :: Bool
     -- camera
     , apt        :: Double
     , foc        :: Double
     , mouseS     :: V3 Double -- mouse    "sensitivity"
     , keyboardRS :: V3 Double -- keyboard "rotation sensitivity"
     , keyboardTS :: V3 Double -- keyboard "translation sensitivity"
     -- object
     , drws       :: [Drawable]
     , selected   :: Bool
     , active     :: Bool
     , backend    :: BackendOptions
     } deriving Show

defaultEntity :: Entity
defaultEntity =
  Entity
  { lable      = "defaultEntity"
  , transform  = defaultTransformable
  , slvrs      = []
  , uuid       = nil
  , parent     = nil
  , parented   = False
  -- camera
  , apt        = 50.0
  , foc        = 100.0
  , mouseS     = -0.0025 -- mouse    "sensitivity"
  , keyboardRS = 0.05    -- keyboard "rotation sensitivity"
  , keyboardTS = 0.05    -- keyboard "translation sensitivity"
  -- object
  , drws       = []
  , selected   = False
  , active     = False
  , backend    = defaultBackendOptions
  } 

initObj :: Entity
initObj =
  defaultEntity
  { transform = defaultTransformable
  , drws     = []
  , selected = False
  , uuid     = nil
  , active   = False
  , lable    = "initObj"
  }

toObject :: [(Texture, TextureObject)] -> [[(Descriptor, R.Material)]]-> PreObject -> IO Entity
toObject txTuples' dms' pobj = do
  --print $ (options pobj)
  let
    dms      = (dms'!!) <$> modelIDXs pobj
    txs      = concatMap (\(_,m) -> R.textures m) $ concat dms :: [Texture]
    txTuples = filter (\(tx,_) -> tx `elem` txs) txTuples'     :: [(Texture, TextureObject)]
    drs =
      toDrawable
      (identity :: M44 Double) -- TODO: add result based on solvers composition
      (options pobj)
      txTuples
      <$> concat dms
      :: [Drawable]
    
    obj =
      defaultEntity
      { transform = defaultTransformable {tslvrs = tsolvers pobj}
      , drws      = drs
      , selected  = False
      , uuid      = puuid   pobj
      , active    = pactive pobj
      , lable     = pname   pobj
      , backend   = options pobj
      , parent    = pparent pobj
      }

  return obj

data PreObject
  =  PreObject
     { pname      :: String
     , ptype      :: PType
     , puuid      :: UUID
     , modelIDXs  :: [Int]
     , tsolvers   :: [Solvable] -- transformable solvers
     , posolvers  :: [Solvable] -- properties solvers
     , options    :: BackendOptions
     , pparent    :: UUID
     , pchildren  :: [PreObject]
     , pactive    :: Bool
     } deriving Show

defaultCam :: Entity
defaultCam =
  defaultEntity
  {
    lable      = "PlayerCamera"
  , apt        = 50.0
  , foc        = 100.0
  , transform  = defaultCamTransformable { tslvrs = [defaultCamSolver]}
  , mouseS     = -0.0025
  , keyboardRS = 0.05
  , keyboardTS = 0.05
  , slvrs      = []
  , uuid       = nil
  , parent     = nil
  }

defaultCamSolver :: Solvable
defaultCamSolver =
  Controllable
  { cvel   = (V3 0 0 0) -- velocity
  , cypr   = (V3 0 0 0) -- rotation
  , cyprS  = (V3 0 0 0) -- sum of rotations
  }

defaultCamTransformable :: Transformable
defaultCamTransformable =
  Transformable
  { xform =  
      (V4
        (V4 1 0 0 0)    -- <- . . . x ...
        (V4 0 1 0 0)    -- <- . . . y ...
        (V4 0 0 1 20)    -- <- . . . z-component of transform
        (V4 0 0 0 1))
  , tslvrs = [Identity]
  }
