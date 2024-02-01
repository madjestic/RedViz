module Graphics.RedViz.Component where

import Linear.V3
import Linear.V4
import Linear.Matrix
import Codec.GlTF.Prelude (Object)
import Data.UUID
import Graphics.RedViz.Drawable
import Graphics.RedViz.Backend (BackendOptions, defaultBackendOptions)

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving Show

data Component = -- TODO: rename Component to Component
    Identity
  | Constant
  | Movable
    { space    :: CoordSys
    , txyz     :: V3 Double -- offset
    , tvel     :: V3 Double -- velocity
    , kinslv   :: [Component]
    } 
  | Turnable
    { space    :: CoordSys
    , cxyz     :: V3 Double -- center of rotation
    , rord     :: RotationOrder
    , rxyz     :: V3 Double
    , avel     :: V3 Double -- angular velocity
    , kinslv   :: [Component]
    }
  | Selectable
    { selected :: Bool}
  | Controllable
    { cvel  :: V3 Double  -- velocity
    , cypr  :: V3 Double  -- yaw/pitch/camRoll ~angular velocity
    , cyprS :: V3 Double  -- yaw/pitch/camRoll Sum
    , mouseS     :: Double -- -0.0025 -- mouse    "sensitivity"
    , keyboardRS :: Double -- 0.05    -- keyboard "rotation sensitivity"
    , keyboardTS :: Double -- 0.05    -- keyboard "translation sensitivity"
    }
  | Parentable
    { parent   :: UUID
    , parented :: Bool }
  | Fadable
    { life :: Double
    , age  :: Double
    , inc  :: Double
    , amp  :: Double
    , func :: Double -> Double
    }
  | Attractable
    { mass :: Double
    , acc  :: V3 Double
    }
  | Transformable
    { xform  :: M44 Double
    , tslvrs :: [Component] }
  | Camerable
    { apt        :: Double -- 50.0
    , foc        :: Double -- 100.0
    }
  | Renderable
    { modelIDXs  :: [Int]
    , drws       :: [Drawable]
    , active     :: Bool
    , backend    :: BackendOptions
    }
  | Debug

-- TODO: add missing Component instances
instance Show Component where
  show Identity
    = "Identity" ++ "\n"
  show Constant
    = "Constant" ++ "\n"
  show Fadable{}
    = "Fadable" ++ "\n"
  show (Movable space' txyz' tvel' kinslvs')
    = "Movable" ++ "\n" 
      ++ "\t" ++ show space' ++ "\n"
      ++ "\t" ++ show txyz'  ++ "\n"
      ++ "\t" ++ show tvel'  ++ "\n"
      ++ "\t" ++ show kinslvs'
  show Turnable{}
    = "Turnable" ++ "\n"
  show (Selectable s)
    = "Selectable, selected :" ++ show s ++ "\n"
  show (Parentable uid p)
    = "Parentable, uid :" ++ show uid ++ "\n"
  show Controllable{}
    = "Controllable" ++ "\n"
  show (Attractable m a)
    = "Attractable :" ++ show m ++ " " ++ show a ++ "\n"
  show (Renderable ms ds a b) =
    "Renderable :" ++ show ms ++ "\n"  ++ show ds ++ "\n"   
  show (Camerable{})
    = "Camerable" ++ "\n"
  show (Transformable xform' tslvrs)
    = "Transformable : " ++ show xform' ++ " " ++ show tslvrs ++ "\n"
  show _
    = "Unknown Solver" ++ "\n"

data RotationOrder =
  XYZ

instance Show RotationOrder where
  show XYZ = "XYZ"

defaultCamTransformable :: Component
defaultCamTransformable =
  Transformable
  { xform =  
      (V4
        (V4 1 0 0 0)    -- <- . . . x ...
        (V4 0 1 0 0)    -- <- . . . y ...
        (V4 0 0 1 20)   -- <- . . . z-component of transform
        (V4 0 0 0 1))
  , tslvrs = [ Identity ]
  }

defaultRenderable :: Component
defaultRenderable = Renderable
  { modelIDXs = []
  , drws      = []
  , active    = False
  , backend   = defaultBackendOptions
  }

defaultTransformable :: Component
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

defaultParentable :: Component
defaultParentable = Parentable
  { parent   = nil
  , parented = False }

defaultCamerable :: Component
defaultCamerable = Camerable
  { apt        = 50.0
  , foc        = 100.0
  }

defaultControllable :: Component
defaultControllable = Controllable
  { cvel  = V3 0 0 0     
  , cypr  = V3 0 0 0
  , cyprS = V3 0 0 0
  , mouseS     = -0.0025 -- mouse    "sensitivity"
  , keyboardRS = 0.05    -- keyboard "rotation sensitivity"
  , keyboardTS = 0.05    -- keyboard "translation sensitivity"
  }
