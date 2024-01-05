--------------------------------------------------------------------------------
-- |
-- Module      :  Mouse
-- Copyright   :  (c) Vladimir Lopatin 2024
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

module Graphics.RedViz.Input.Mouse.Lens
  ( Mouse (..)
  , pos
  , rpos
  , mmov
  , rmb
  , lmb
  ) where

--import Lens.Micro
import Lens.Micro.TH

import Linear.V3

data Mouse
  =  Mouse
  { -- | Mouse State
    _lmb   :: Maybe (Int, Int)
  , _rmb   :: Maybe (Int, Int)
  , _pos  ::        (Int, Int)
  , _rpos ::        (Int, Int)
  , _mmov ::        Bool
  , mVecs ::        [V3 Int]
  } deriving Show

$(makeLenses ''Mouse)
