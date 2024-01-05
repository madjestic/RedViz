--------------------------------------------------------------------------------
-- |
-- Module      :  Model
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
{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Project.Model 
  ( Model (..)
  , path
  ) where

--import Lens.Micro
import Lens.Micro.TH
import Data.Aeson
import Data.Aeson.TH


data Model
  =  Model
     {
       _path :: String
     } deriving Show

$(makeLenses ''Model)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Model
