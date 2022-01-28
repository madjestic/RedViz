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
-- A structure for a user-controllable object.
--
--------------------------------------------------------------------------------


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.RedViz.Controllable
  ( Controllable (..)
  , Device (..)
  , Keyboard (..)
  , Mouse (..)
  , transform
--  , transform'
  , ypr
  , vel
  , device
  , device'
  , mouse
  , keyboard
  ) where

import Linear.Matrix
import Linear.V3
import Control.Lens hiding (transform)

import Graphics.RedViz.Keyboard
import Graphics.RedViz.Mouse
import Graphics.RedViz.Utils ()

-- import Debug.Trace as DT

data Controllable
  =  Controller
     {
       _debug      :: (Double, Double)
     , _transform  :: M44 Double
     , _vel        :: V3 Double  -- velocity
     , _ypr        :: V3 Double  -- yaw/pitch/roll
     , _device     :: Device     -- store as index in the proj file: 0 - keyboard, 1 - mouse, etc.
     }
--   |  Solver
--      {
-- --       _pivot      :: V3 Double
--        _transform  :: M44 Double
--      , _ypr        :: V3 Double  -- yaw/pitch/roll
-- --     , _velocity   :: V3 Double
-- --     , _physC      :: Physics -- TODO : add phys.parms
--      }
  deriving Show

data Device
  =  Device
     {
       _keyboard :: Keyboard
     , _mouse    :: Mouse
     } deriving Show

-- transform' :: Lens' Controllable (M44 Double)
-- transform' = lens _transform (\controllable newTransform -> Solver { _transform = newTransform })

device'    :: Lens' Controllable Device
device'    = lens _device (\controllable newDevice    -> controllable { _device    = newDevice })

$(makeLenses ''Device)
$(makeLenses ''Controllable)
