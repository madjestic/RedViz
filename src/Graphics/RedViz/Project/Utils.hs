module Graphics.RedViz.Project.Utils
  ( fromProjectCamera
  ) where

import Linear.V3

import Graphics.RedViz.Project.Project
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable
import Graphics.RedViz.Utils

fromProjectCamera :: ProjectCamera -> Camera
fromProjectCamera pcam =
  defaultCam
  {
    _apt        = _pApt pcam
  , _foc        = _pFoc pcam
  , _controller =
      defaultCamController
      { _transform = fromList ( _pTransform pcam) }
  , _mouseS     = pure $ _pMouseS pcam     :: V3 Double
  , _keyboardRS = pure $ _pKeyboardRS pcam :: V3 Double
  , _keyboardTS = pure $ _pKeyboardTS pcam :: V3 Double
  }
