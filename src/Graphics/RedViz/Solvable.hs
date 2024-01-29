module Graphics.RedViz.Solvable where

import Linear.V3
import Codec.GlTF.Prelude (Object)
import Data.UUID

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving Show

data Solvable = -- TODO: rename Solvable to Component
    Identity
  | Constant
  | Movable
    { space    :: CoordSys
    , txyz     :: V3 Double -- offset
    , tvel     :: V3 Double -- velocity
    , kinslv   :: [Solvable]
    } 
  | Turnable
    { space    :: CoordSys
    , cxyz     :: V3 Double -- center of rotation
    , rord     :: RotationOrder
    , rxyz     :: V3 Double
    , avel     :: V3 Double -- angular velocity
    , kinslv   :: [Solvable]
    }
  | Selectable
  | Controllable
    { cvel  :: V3 Double  -- velocity
    , cypr  :: V3 Double  -- yaw/pitch/camRoll ~angular velocity
    , cyprS :: V3 Double  -- yaw/pitch/camRoll Sum
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

instance Show Solvable where
  show Identity           = "Identity"
  show Fadable{}          = "Fadable"
  show Movable{}          = "Movable"
  show Turnable{}         = "Turnable"
  show Selectable         = "Selectable"
  show (Parentable uid p) = "Parentable, uid :" ++ show uid
  show Controllable{}     = "Controllable"
  show (Attractable m a)  = "Attractable :" ++ show m ++ " " ++ show a
  show _                  = "Unknown Solver"

data RotationOrder =
  XYZ

instance Show RotationOrder where
  show XYZ = "XYZ"
