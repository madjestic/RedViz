{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Widget
  ( Alignment (..)
  , Format (..)
  , Widget (..)
  , BBox (..)
  , xoffset
  , yoffset
  , alignment
  , soffset
  , zoffset
  , ssize
  , text
  , lable
  , bbox
  , rover
  , pressed
  , isPressed
  , coords
  , defaultFormat
  ) where

import Control.Lens
import Graphics.RedViz.Backend

data Alignment =
   TL |TC |TR
  |CL |CC |CR
  |BL |BC |BR
  deriving Show

data Format -- move to Format.hs?
  =  Format
     { _alignment :: Alignment
     , _xoffset   :: Double
     , _yoffset   :: Double
     , _zoffset   :: Double
     , _soffset   :: Double -- scale Offset
     , _ssize     :: Double -- scale Size
     } deriving Show
$(makeLenses ''Format)

defaultFormat :: Format
defaultFormat = Format CC 0.0 0.0 0.0 0.085 1.0

data BBox
  =  BBox
     { -- TL BR
       _bbx0 :: Double
     , _bby0 :: Double
     , _bbx1 :: Double
     , _bby1 :: Double
     } deriving Show
$(makeLenses ''BBox)

data Widget
  =  TextField
     { _active  :: Bool
     , _text    :: [String]
     , _format  :: Format
     , _options :: BackendOptions
     }
  |  FPS
     { _active  :: Bool
     , _format  :: Format
     , _options :: BackendOptions
     }
  |  Button -- True when pressed, False when released
     { _active  :: Bool
     , _lable   :: String
     , _bbox    :: BBox
     , _rover   :: Bool
     , _pressed :: Bool
     , _format  :: Format
     , _options :: BackendOptions     
     }
  |  Toggle -- same as Buttong, but stays True, untill pressed again
     { _active  :: Bool
     , _lable   :: String
     , _bbox    :: BBox
     , _over    :: Bool
     , _pressed :: Bool
     , _format  :: Format
     , _options :: BackendOptions     
     }
  |  MultiToggle
     { _active  :: Bool
     , _current :: Int
     , _toggles :: [Widget]
     , _options :: BackendOptions     
     }
  |  Cursor
     { _active  :: Bool
     , _lable   :: String -- to show tooltips
     , _coords  :: (Double, Double) -- (mouse) pos -> update and draw
     , _options :: BackendOptions     
     }
$(makeLenses ''Widget)

instance Show Widget where
  show (TextField b t f opts)     = show "TextField :"   ++ show (b, t, f, opts)
  show (FPS b f opts)             = show "FPS :"         ++ show (b, f, opts)
  show (Button a l bb o p f opts) = show "Button :"      ++ show (a, l, bb, o, p, f, opts)
  show (Cursor a l xy opts)       = show "Cursor :"      ++ show (a, l, xy, opts)
  show (Toggle a l bb o p f opts) = show "Toggle :"      ++ show (a, l, bb, o, p, f, opts)
  show (MultiToggle a c ts opts)  = show "MultiToggle :" ++ show (a, c, ts, opts)

isPressed :: Maybe Widget -> Bool
isPressed wgt = 
  case wgt of
    Just w  -> _pressed w
    Nothing -> False
