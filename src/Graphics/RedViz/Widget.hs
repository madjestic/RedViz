--------------------------------------------------------------------------------
-- |
-- Module      :  Widget
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic camera structure.
--
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Graphics.RedViz.Widget
  ( Alignment (..)
  , Widget (..)
  ) where

import GHC.Generics
import SDL (Point)
import Linear.V2
import Foreign.C
import Data.Binary
--import GHC.Generics

import Graphics.RedViz.Backend
import Graphics.RedViz.Drawable
import Graphics.RedViz.Entity
import Graphics.RedViz.Utils ()

data Widget
  =  Empty
  |  TextField
     { active   :: Bool
     , text     :: [String]
     , fonts    :: [Object]
     , format   :: Format
     , optionsW :: Options
     }
  |  Cursor
     { active   :: Bool
     , icons    :: [Object]
     , cpos     :: Point V2 Int
     , format   :: Format     
     , optionsW :: Options
     }
  |  Gizmo
     { active   :: Bool
     , icons    :: [Object]
     , cpos     :: Point V2 Int
     , format   :: Format     
     , optionsW :: Options
     }
  |  Selector
     { active  :: Bool
     , icons   :: [Object]
     , objects :: [Entity]
     , format  :: Format
     }
  |  InfoField
     { active   :: Bool
     , text     :: [String]
     , fonts    :: [Object]
     , format   :: Format
     , optionsW :: Options
     }
  deriving (Show, Generic, Binary)
