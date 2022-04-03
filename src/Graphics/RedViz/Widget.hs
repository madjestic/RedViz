{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Widget
  ( Alignment (..)
  , Format (..)
  , Widget (..)
  , hoffset
  , voffset
  , alignment
  , soffset
  , ssize
  , text
  , defaultFormat
  ) where

import Control.Lens

data Alignment =
   TL |TC |TR
  |CL |CC |CR
  |BL |BC |BR
  deriving Show

data Format -- move to Format.hs?
  =  Format
     { _alignment :: Alignment
     , _voffset   :: Double
     , _hoffset   :: Double
     , _soffset   :: Double -- scale Offset
     , _ssize     :: Double -- scale Size
     } deriving Show
$(makeLenses ''Format)

defaultFormat :: Format
defaultFormat = Format CC 0.0 (-0.4) 0.085 1.0

data Widget
  =  TextField
     { _active :: Bool
     , _text   :: [String]
     , _format :: Format
     }
  |  FPS
     { _active :: Bool
     , _format :: Format}
$(makeLenses ''Widget)

instance Show Widget where
  show (TextField b f t) = show "TextField" ++ show (b, f, t)
  show (FPS b f)         = show "FPS" ++ show (b, f)
