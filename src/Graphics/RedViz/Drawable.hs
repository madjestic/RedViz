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

module Graphics.RedViz.Drawable where

import Graphics.Rendering.OpenGL (TextureObject)
import Graphics.RedViz.Backend (BackendOptions)
import Linear.Matrix (M44)

import Graphics.RedViz.Descriptor
import Graphics.RedViz.Material
import Graphics.RedViz.Texture

data Drawable
  =  Drawable
     { descriptor :: Descriptor
     , material   :: Material
     , dtxs       :: [(Int, (Texture, TextureObject))]
     , doptions   :: BackendOptions
     , u_xform    :: M44 Double
     } deriving Show  
