module Graphics.RedViz.Component where

import Linear.V3
import Linear.V4
import Linear.Matrix
import Data.UUID
import Graphics.RedViz.Drawable
import Graphics.RedViz.Backend (Options, defaultOptions)

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving Show

data Component = -- TODO: rename Component to Component
    Identity
  | Constant
  | Movable
    { space    :: CoordSys
    , tvel     :: V3 Double -- velocity
    , kslvrs   :: [Component]
    } 
  | Turnable
    { space    :: CoordSys
    , rord     :: RotationOrder
    , cxyz     :: V3 Double -- center of rotation
    , rxyz     :: V3 Double -- sum of rotations
    , avel     :: V3 Double -- angular velocity
    , kslvrs   :: [Component]
    }
  | Selectable
    { selected :: Bool }
  | Controllable
    { cvel    :: V3 Double -- velocity
    , cypr    :: V3 Double -- yaw/pitch/camRoll ~angular velocity
    , cyprS   :: V3 Double -- yaw/pitch/camRoll Sum
    , mouseS  :: Double    -- -0.0025 -- mouse    "sensitivity"
    , rotS    :: Double    -- 0.05    -- keyboard "rotation sensitivity"
    , movS    :: Double    -- 0.05    -- keyboard "translation sensitivity"
    , parent  :: UUID
    , phys    :: Physics
    }
  | Parentable
    { parent   :: UUID
    }
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
    , fr   :: Double -- rotation    force scalar
    , ft   :: Double -- translation force scalar
    }
  | Measurable
    { mass :: Double }
  | Transformable
    { xform  :: M44 Double
    , tslvrs :: [Component] }
  | PreTransformable
    { txyz     :: V3 Double -- offset
    , rord     :: RotationOrder
    , rxyz     :: V3 Double
    }
  | Camerable
    { apt        :: Double -- 50.0
    , foc        :: Double -- 100.0
    }
  | Renderable
    { modelIDXs  :: [Int]
    , drws       :: [Drawable]
    , active     :: Bool
    , backend    :: Options
    }

data Physics =
    Static
  | Dynamic
    deriving Show

-- TODO: add missing Component instances
instance Show Component where
  show Identity
    = "Identity" ++ "\n"
  show Constant
    = "Constant" ++ "\n"
  show Fadable{}
    = "Fadable" ++ "\n"
  show (Measurable m')
    = "Measurable" ++ "\n"
    ++ "\t" ++ show m' ++ "\n"
    ++ "/////////////////////////" ++ "\n"
  show (Movable space' tvel' kinslvs')
    = "Movable" ++ "\n" 
      ++ "\t" ++ show space' ++ "\n"
      ++ "\t" ++ show tvel'  ++ "\n"
      ++ "\t" ++ show kinslvs' ++ "\n"
      ++ "/////////////////////////" ++ "\n"
  show Turnable{}
    = "Turnable" ++ "\n"
  show (Selectable s)
    = "Selectable, selected :" ++ show s ++ "\n"
  show (Parentable uid)
    = "Parentable, uid :" ++ "\n"
    ++ "\t" ++ show uid   ++ "\n"
  show (Controllable cvel' cypr' cyprS' mouseS' keyboardRS' keyboardTS' parent' phsx') -- TODO: add debug info
    = "Controllable" ++ "\n"
      ++ "\t" ++ show cvel'       ++ "\n"
      ++ "\t" ++ show cypr'       ++ "\n"
      ++ "\t" ++ show cyprS'      ++ "\n"
      ++ "\t" ++ show mouseS'     ++ "\n"      
      ++ "\t" ++ show keyboardRS' ++ "\n"      
      ++ "\t" ++ show keyboardTS' ++ "\n"      
      ++ "\t" ++ show parent'     ++ "\n"
      ++ "\t" ++ show phsx'     ++ "\n"      
      ++ "//////////////////////////////" ++ "\n"
  show (Attractable m a fr ft)
    = "Attractable : " ++ show m ++ " " ++ show a ++ " " ++ "\n"
  show (Renderable ms ds _ _) =
    "Renderable :" ++ show ms ++ "\n"  ++ show ds ++ "\n"   
  show (Camerable{})
    = "Camerable" ++ "\n"
  show (Transformable xform' tslvrs)
    = "Transformable : " ++ show xform' ++ " " ++ show tslvrs ++ "\n"
  show (PreTransformable txyz' rord' rxyz')
    = "PreTransformable" ++ "\n"
      ++ "\t" ++ show txyz'  ++ "\n"
      ++ "\t" ++ show rord'  ++ "\n"      
      ++ "\t" ++ show txyz'  ++ "\n"      
      ++ "//////////////////////////////" ++ "\n"

data RotationOrder =
  XYZ
  deriving Show

defaultCamTransformable :: Component
defaultCamTransformable =
  Transformable
  { xform =  
      (V4
        (V4 1 0 0 0)    -- <- . . . x ...
        (V4 0 1 0 0)    -- <- . . . y ...
        (V4 0 0 1 30)   -- <- . . . z-component of transform
        (V4 0 0 0 1))
  , tslvrs = [ Identity ]
  }

defaultRenderable :: Component
defaultRenderable = Renderable
  { modelIDXs = []
  , drws      = []
  , active    = False
  , backend   = defaultOptions
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
  -- , parented = False
  -- , active   = False
  }

defaultCamerable :: Component
defaultCamerable = Camerable
  { apt        = 50.0
  , foc        = 100.0
  }

defaultControllable :: Component
defaultControllable = Controllable
  { cvel   = V3 0 0 0     
  , cypr   = V3 0 0 0
  , cyprS  = V3 0 0 0
  , mouseS = -0.0000025 -- mouse    "sensitivity"
  , rotS   =  0.0005    -- keyboard "rotation sensitivity"
  , movS   =  0.1       -- keyboard "translation sensitivity"
  , parent = nil
  , phys   = Static
  }

defaultAttractable = Attractable
  { mass = 0.0
  , acc  = V3 0 0 0
  , fr   = 1.0
  , ft   = 1.0
  }

