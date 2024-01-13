module Graphics.RedViz.Solvable where

import Linear.V3
import Codec.GlTF.Prelude (Object)

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving Show

data Solvable =
    Identity
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
  | ParentableToPlayer
  | Fadable
    { life :: Double
    , age  :: Double
    , inc  :: Double
    , amp  :: Double
    , func :: Double -> Double
    }
  | Pullable
    { mass :: Double
    , acc  :: V3 Double
    }

instance Show Solvable where
  show Fadable{}      = "Fadable"
  show Identity     = "Identity"
  show Movable{}  = "Movable"
  show Turnable{}     = "Turnable"
  show Selectable       = "Selectable"
  show Parentable       = "Parentable"
  show Controllable{} = "Controllable"

data RotationOrder =
  XYZ

instance Show RotationOrder where
  show XYZ = "XYZ"
