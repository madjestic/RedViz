{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Object
  ( Object' (..)
  , descriptors
  , materials
  , programs
  , transforms
  , time
  , ypr0
  , ypr
  , defaultObject'
  ) where

import Control.Lens hiding (transform, pre)
import Graphics.Rendering.OpenGL (Program)
import Linear.Matrix (M44)
import Linear.V3

import Graphics.RedViz.Descriptor
import Graphics.RedViz.Material

data Object'
  =  Object'
     {
       _descriptors :: [Descriptor] -- | Material is bound in Descriptor, but we also use this data for draw-call separation per material.
                -- data Descriptor =
                     -- Descriptor VertexArrayObject NumArrayIndices
      , _materials   :: [Material]   -- | hence [Material] is present on the Object level too, we use that value, instead of looking it up from respective VGeo.
      , _programs    :: [Program]    -- | Shader Programs
      , _transforms  :: ![M44 Double]
      , _ypr0        :: V3 Double
      , _ypr         :: !(V3 Double)
      , _time        :: Double
     } deriving Show
$(makeLenses ''Object')

zeroV3 :: V3 Double
zeroV3 = V3 0 0 0

defaultObject' :: Object'
defaultObject' = Object' [] [] [] [] zeroV3 zeroV3 0.0
