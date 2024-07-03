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

module Graphics.RedViz.Widget
  ( Alignment (..)
  , Widget (..)
  ) where

import GHC.Generics
import SDL (Point)
import Linear.V2
import Foreign.C

import Graphics.RedViz.Backend
import Graphics.RedViz.Drawable
import Graphics.RedViz.Entity

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
     , icons    :: [Object] -- ~[Objectable]
     , cpos     :: Point V2 CInt
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
  deriving (Generic, Show)
