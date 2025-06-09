module Graphics.RedViz.AppInput where

import SDL 
import Data.IORef
import Control.Concurrent.STM
import Graphics.Rendering.OpenGL (GLfloat)
import Linear.V3

import Graphics.RedViz.Game (GameSettings)

data Cont = Play
          | New
          | Load
          | Quit
  deriving (Show, Eq)

data AppInput = AppInput
  { window    :: Maybe Window
  , mpos      :: V2 Double
  , mposRef   :: IORef (V3 Double)
  , settings  :: GameSettings
  , menuApp   :: Bool
  , pauseApp  :: Bool
  , qsave     :: Bool
  , qload     :: Bool
  , evnts     :: [Event]
  , evntsRef  :: TVar [Event]
  , cont      :: Cont
  , contRef   :: TVar Cont
  , saveMenu  :: Int
  , saveMenuRef :: IORef Int
  , slider    :: Float
  , sliderRef :: IORef Float
  --- debugging shadowMap
  --, sliderLightRayDirection    :: V3 GLfloat
  , sliderLightRayDirectionRef :: IORef (Float, Float, Float)
  , eyeRef    :: IORef (Float, Float, Float)
  , centerRef :: IORef (Float, Float, Float)
  , upRef     :: IORef (Float, Float, Float)
  , lrbRef    :: IORef (Float, Float, Float)
  , tnfRef    :: IORef (Float, Float, Float)
  , scalarRef :: IORef Float
  } deriving Show

instance Show (IORef (V3 Double)) where
  show = show

instance Show (IORef (Float, Float, Float)) where
  show = show

instance Show (IORef Float) where
  show = show  

instance Show (IORef Int) where
  show = show  

instance Show (TVar Cont) where
  show = show  

instance Show (TVar [Event]) where
  show = show  
  
