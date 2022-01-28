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
  ) where

import Graphics.RedViz.Material
import Graphics.RedViz.Descriptor
import Graphics.Rendering.OpenGL (Program)
import Foreign.C
import Linear.Matrix
import Control.Lens

data Drawable
  =  Drawable
     {  name       :: String
     , _uniforms   :: Uniforms
     , _descriptor :: Descriptor
     , _program    :: Program
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
