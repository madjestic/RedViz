--------------------------------------------------------------------------------
-- |
-- Module      :  Mouse
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic mouse control structure.
--
--------------------------------------------------------------------------------


{-# LANGUAGE TemplateHaskell, Arrows #-}

module Graphics.RedViz.Mouse
  ( Mouse (..)
  , pos
  , rpos
  , mmov
  ) where

import Control.Lens

import Linear.V3

data Mouse
  =  Mouse
  { -- | Mouse State
    _lmb   :: Maybe (Double, Double)
  --, mmb
  , _rmb   :: Maybe (Double, Double)
  , _pos  ::       (Double, Double)
  , _rpos ::       (Double, Double)
  , _mmov ::        Bool
  , mVecs ::       [V3 Double]
  } deriving Show

$(makeLenses ''Mouse)

