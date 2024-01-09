module Graphics.RedViz.Transformable where

import Linear.Matrix
import Linear.V4

import Graphics.RedViz.Solvable

data Transformable
  =  Transformable
     { xform  :: M44 Double
     , tslvrs :: [Solvable]
     } deriving Show

defaultTransformable :: Transformable
defaultTransformable =
  Transformable
  { xform =  
      (V4
        (V4 1 0 0 0)   -- <- . . . x ...
        (V4 0 1 0 0)   -- <- . . . y ...
        (V4 0 0 1 0)   -- <- . . . z-component of transform
        (V4 0 0 0 1))
  , tslvrs = [Identity]
  }
