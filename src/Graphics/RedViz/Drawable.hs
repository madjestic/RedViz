--------------------------------------------------------------------------------
-- |
-- Module      :  Drawable
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Drawable data type and related structures.
--
--------------------------------------------------------------------------------


{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Drawable
  ( uniforms
  , u_xform
  , Drawable (..)
  , Uniforms (..)
  , toDrawables
  ) where

import Foreign.C
import Linear.Matrix
import Control.Lens

import Graphics.RedViz.Controllable as Controllable
import Graphics.RedViz.Material
import Graphics.RedViz.Camera
import Graphics.RedViz.Object as Object
import Graphics.RedViz.Descriptor
import Graphics.Rendering.OpenGL (Program)
import Graphics.RedViz.Backend

data Drawable
  =  Drawable
     {  name       :: String
     , _uniforms   :: Uniforms
     , _descriptor :: Descriptor
     --, _program    :: Program
     , _options    :: BackendOptions
     } deriving Show

data Uniforms
  =  Uniforms
     {
       _u_mats  :: Material
     , _u_prog  :: Program
     , _u_mouse :: (Double, Double)
     , _u_time  :: Double
     , _u_res   :: (CInt, CInt)
     --, u_proj  :: M44 Double --GLmatrix GLfloat
     , _u_cam   :: M44 Double
     , _u_cam_a :: Double
     , _u_cam_f :: Double
     , _u_xform :: M44 Double
     } deriving Show

$(makeLenses ''Drawable)
$(makeLenses ''Uniforms)

toDrawables
  :: (Double, Double)
  -> Double
  -> (CInt, CInt)
  -> Camera
  -> Object' -> [Drawable]
toDrawables mpos time0 res0 cam obj = drs
  where
    drs = toDrawable name' mpos time0 res0 cam xformO opts'
          <$> [(mats, progs, ds)
              | mats  <- obj ^. materials
              , progs <- obj ^. programs
              , ds    <- obj ^. descriptors]

    name'  = obj ^. Object.name
    xformO = obj ^. transform0
    opts'  = obj ^. Object.options :: BackendOptions

type MousePos    = (Double, Double)
type Time        = Double
type Res         = (CInt, CInt)
type CameraM44   = M44 Double
type ViewAngle   = Double
type FieldOfView = Double

toDrawable ::
     String
  -> MousePos
  -> Time
  -> Res
  -> Camera
  -> M44 Double
  -> BackendOptions
  -> (Material, Program, Descriptor)
  -> Drawable
toDrawable name' mpos time' res' cam xformO opts (mat, prg, d) = dr
  where
    apt    = _apt cam
    foc    = _foc cam
    xformC = view (controller . Controllable.transform) cam  :: M44 Double
    dr  = Drawable name' (Uniforms mat prg mpos time' res' xformC apt foc xformO) d opts
