--{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Object
  ( Object' (..)
  , defaultObject'
  ) where

--import Control.Lens hiding (transform, pre)
import Graphics.Rendering.OpenGL (Program)
import Linear.Matrix
import Linear.V3

import Graphics.RedViz.Descriptor
import Graphics.RedViz.Material hiding (name)
import Graphics.RedViz.Backend

data Object'
  =  Object'
     {
        name        :: String
      , descriptors :: [Descriptor] -- | Material is bound in Descriptor, but we also use this data for draw-call separation per material.
       -- data Descriptor =
       -- Descriptor VertexArrayObject NumArrayIndices
      , materials   :: [Material]    -- | hence [Material] is present on the Object level too, we use that value, instead of looking it up from respective VGeo.
      , programs    :: [Program]     -- | Shader Programs
      , transforms  :: ![M44 Double] -- | transforms for parts (object fragments)
      , transform0  :: !(M44 Double) -- | initial basis (position/orientation in world space)
      , transform1  :: !(M44 Double) -- | basis (position/orientation in world space)
      , ypr0        :: !(V3 Double)
      , ypr         :: !(V3 Double)
      , time        :: Double
      , options     :: BackendOptions
     } deriving Show
-- $(makeLenses ''Object')

zeroV3 :: V3 Double
zeroV3 = V3 0 0 0

-- defaultObject' :: Object'
-- defaultObject' = Object' [] [] [] [] (identity::M44 Double) zeroV3 zeroV3 0.0
defaultObject' :: Object'
defaultObject' =
  Object'
  {
    descriptors = []
  , materials   = []
  , programs    = []
  , transforms  = []
  , transform0  = identity :: M44 Double
  , transform1  = identity :: M44 Double
  , ypr0        = zeroV3
  , ypr         = zeroV3
  , time        = 0.0
  , options     = defaultBackendOptions
  , name        = "defaultObject'"
  }

