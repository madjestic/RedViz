--------------------------------------------------------------------------------
-- |
-- Module      :  Drawable
-- Copyright   :  (c) Vladimir Lopatin 2024
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

module Graphics.RedViz.Drawable.Lens
  ( uniforms
  , u_xform
  , Drawable (..)
  , Uniforms (..)
  , toDrawables
  , toDrawable
  , options
  , descriptor
  , u_cam
  , u_cam_a
  , u_cam_accel
  , u_cam_f
  , u_cam_vel
  , u_cam_ypr
  , u_cam_yprS
  , u_res
  , u_time
  ) where

import Foreign.C
import Linear.Matrix
import Linear.V3
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.TH

import Graphics.RedViz.Controllable as Controllable
import Graphics.RedViz.Camera.Lens
import qualified Graphics.RedViz.Object.Lens as Object
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Backend

--import Debug.Trace    as DT

data Drawable
  =  Drawable
     {  name       :: String
     , _uniforms   :: Uniforms
     , _descriptor :: Descriptor
     , _options    :: BackendOptions
     } deriving Show

data Uniforms
  =  Uniforms
     {
       _u_time  :: Double
     , _u_res   :: (CInt, CInt)
     , _u_cam   :: M44 Double
     , _u_cam_a :: Double
     , _u_cam_f :: Double
     , _u_xform :: M44 Double
     , _u_cam_ypr   :: (Double, Double, Double)
     , _u_cam_yprS  :: (Double, Double, Double)
     , _u_cam_vel   :: (Double, Double, Double)
     , _u_cam_accel :: (Double, Double, Double)
     } deriving Show

$(makeLenses ''Drawable)
$(makeLenses ''Uniforms)

toDrawables
  :: Double
  -> (CInt, CInt)
  -> Camera
  -> Object.Object' -> [Drawable]
toDrawables time0 res0 cam obj = drs
  where
    drs = toDrawable name' time0 res0 cam xformO opts'
          <$> obj ^. Object.descriptors

    name'  = obj ^. Object.name
    xformO = obj ^. Object.transform0
    opts'  = obj ^. Object.options :: BackendOptions

type Time        = Double
type Res         = (CInt, CInt)

toDrawable ::
     String
  -> Time
  -> Res
  -> Camera
  -> M44 Double
  -> BackendOptions
  -> Descriptor
  -> Drawable
toDrawable name' time' res' cam xformO opts d = dr
  where
    apt'   = _apt cam
    foc'   = _foc cam
    xformC = view (controller . Controllable.transform) cam  :: M44 Double
    dr  =
      Drawable
      {
        name     = name'
      ,_uniforms =
          Uniforms
          {
            _u_time  = time'
          , _u_res   = res'
          , _u_cam   = xformC
          , _u_cam_a = apt'
          , _u_cam_f = foc'
          , _u_xform = xformO
          , _u_cam_ypr   = (\(V3 x y z) -> (x,y,z)) $ cam ^. controller . Controllable.ypr
          , _u_cam_yprS  = (\(V3 x y z) -> (x,y,z)) $ cam ^. controller . Controllable.yprS
          , _u_cam_vel   = (\(V3 x y z) -> (x,y,z)) $ cam ^. controller . Controllable.vel
          , _u_cam_accel = (0,0,0)
          }
      ,_descriptor = d
      ,_options    = opts
      }
