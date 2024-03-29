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


module Graphics.RedViz.Input.Mouse
  ( Mouse (..)
  ) where

import Linear.V3

data Mouse
  =  Mouse
  { -- | Mouse State
    lmb   :: Maybe (Int, Int)
  , rmb   :: Maybe (Int, Int)
  , pos  ::        (Int, Int)
  , rpos ::        (Int, Int)
  , mmov ::        Bool
  , mVecs ::       [V3 Int]
  } deriving Show
