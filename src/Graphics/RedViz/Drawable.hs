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

--{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Drawable
  ( Drawable (..)
  , Uniforms (..)
  , toDrawables
  , toDrawable
  ) where

import Foreign.C
import Linear.Matrix
import Linear.V3
--import Control.Lens

import Graphics.RedViz.Controllable as Controllable
--import Graphics.RedViz.Material
import Graphics.RedViz.Camera
import qualified Graphics.RedViz.Object as Object
import Graphics.RedViz.Descriptor
import Graphics.Rendering.OpenGL (Program)
import Graphics.RedViz.Backend

--import Debug.Trace    as DT

data Drawable
  =  Drawable
     { name       :: String
     , uniforms   :: Uniforms
     , descriptor :: Descriptor
     , program    :: Program
     , options    :: BackendOptions
     } deriving Show

data Uniforms
  =  Uniforms
     {
       u_time  :: Double
     , u_res   :: (CInt, CInt)
     , u_cam   :: M44 Double
     , u_cam_a :: Double
     , u_cam_f :: Double
     , u_xform :: M44 Double
     , u_cam_ypr   :: (Double, Double, Double)
     , u_cam_yprS  :: (Double, Double, Double)
     , u_cam_vel   :: (Double, Double, Double)
     , u_cam_accel :: (Double, Double, Double)
     } deriving Show

toDrawables
  :: Double
  -> (CInt, CInt)
  -> Camera
  -> Object.Object' -> [Drawable]
toDrawables time0 res0 cam obj = drs
  where
    drs = toDrawable name' time0 res0 cam xformO opts'
          <$> zip
          (Object.programs    obj)
          (Object.descriptors obj)

    name'  = Object.name       obj
    xformO = Object.transform0 obj
    opts'  = Object.options    obj :: BackendOptions

type Time        = Double
type Res         = (CInt, CInt)
type CameraM44   = M44 Double
type ViewAngle   = Double
type FieldOfView = Double

toDrawable ::
     String
  -> Time
  -> Res
  -> Camera
  -> M44 Double
  -> BackendOptions
  -> (Program, Descriptor)
  -> Drawable
toDrawable name' time' res' cam xformO opts (prg, d) = dr
  where
    apt'    = apt cam
    foc'    = foc cam
    --xformC =  (controller . Controllable.transform) cam  :: M44 Double
    xformC =  _transform (controller cam) :: M44 Double
    --xformC =  undefined :: M44 Double
    dr  =
      Drawable
      {
        Graphics.RedViz.Drawable.name = name'
      , uniforms   =
          Uniforms
          {
            u_time  = time'
          , u_res   = res'
          , u_cam   = xformC
          , u_cam_a = apt'
          , u_cam_f = foc'
          , u_xform = xformO
          , u_cam_ypr   = (\(V3 x y z) -> (x,y,z)) $ _ypr  (controller cam)
          , u_cam_yprS  = (\(V3 x y z) -> (x,y,z)) $ _yprS (controller cam)
          , u_cam_vel   = (\(V3 x y z) -> (x,y,z)) $ _vel  (controller cam)
          , u_cam_accel = (0,0,0)
          }
      , descriptor = d
      , program    = prg
      , options    = opts
      }
