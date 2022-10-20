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
     { _active :: Bool
     , _text   :: [String]
     , _format :: Format
     }
  |  FPS
     { _active :: Bool
     , _format :: Format}
  |  Button -- True when pressed, False when released
     { _active  :: Bool
     , _lable   :: String
     , _bbox    :: BBox
     , _rover   :: Bool
     , _pressed :: Bool
     , _format  :: Format
     }
  |  Toggle -- same as Buttong, but stays True, untill pressed again
     { _active  :: Bool
     , _lable   :: String
     , _bbox    :: BBox
     , _over    :: Bool
     , _pressed :: Bool
     , _format  :: Format
     }
  |  MultiToggle
     { _active  :: Bool
     , _current :: Int
     , _toggles :: [Widget]
     }
  |  Cursor
     { _active  :: Bool
     , _lable   :: String -- to show tooltips
     , _coords  :: (Double, Double) -- (mouse) pos -> update and draw
     }
$(makeLenses ''Widget)

instance Show Widget where
  show (TextField b t f)     = show "TextField :"   ++ show (b, t, f)
  show (FPS b f)             = show "FPS :"         ++ show (b, f)
  show (Button a l bb o p f) = show "Button :"      ++ show (a, l, bb, o, p, f)
  show (Cursor a l xy)       = show "Cursor :"      ++ show (a, l, xy)
  show (Toggle a l bb o p f) = show "Toggle :"      ++ show (a, l, bb, o, p, f)
  show (MultiToggle a c ts)  = show "MultiToggle :" ++ show (a, c, ts)

isPressed :: Maybe Widget -> Bool
isPressed wgt = 
  case wgt of
    Just w  -> _pressed w
    Nothing -> False
