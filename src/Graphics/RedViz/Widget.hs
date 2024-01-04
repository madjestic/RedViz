{-# LANGUAGE DeriveGeneric #-}

module Graphics.RedViz.Widget
  ( Alignment (..)
  , Widget (..)
  ) where

import Lens.Micro hiding (Empty)
import Lens.Micro.TH
import GHC.Generics
import SDL (Point)
import Linear.V2
import Foreign.C

import Graphics.RedViz.Backend
import Graphics.RedViz.Object
import Graphics.RedViz.Drawable

data Widget
  =  Empty
  |  TextField
     { active   :: Bool
     , text     :: [String]
     , fonts    :: [Object]
     , format   :: Format
     , optionsW :: BackendOptions
     }
  |  Cursor
     { active   :: Bool
     , icons    :: [Object]     
     , cpos     :: Point V2 CInt
     , optionsW :: BackendOptions
     }
  |  Selector
     { active  :: Bool
     , icons   :: [Object]
     , objects :: [Object]
     }
  deriving (Generic, Show)
