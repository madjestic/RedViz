module Graphics.RedViz.Solvable where

import Linear.V3

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving Show

data Solvable =
    Identity
  | Translate
    { space    :: CoordSys
    , txyz     :: V3 Double -- offset
    , tvel     :: V3 Double -- velocity
    , kinslv   :: Solvable
    } 
  | Rotate
    { space    :: CoordSys
    , cxyz     :: V3 Double -- center of rotation
    , rord     :: RotationOrder
    , rxyz     :: V3 Double
    , avel     :: V3 Double -- angular velocity
    , kinslv :: Solvable
    }
  | Select
  | Controller
    { cvel  :: V3 Double  -- velocity
    , cypr  :: V3 Double  -- yaw/pitch/camRoll ~angular velocity
    , cyprS :: V3 Double  -- yaw/pitch/camRoll Sum
    }
  | Parent
    -- | Parent
  --   { sertParent :: Object | Camera}
  | Speed
    { life :: Double
    , age  :: Double
    , inc  :: Double
    , amp  :: Double
    , func :: Double -> Double
    }
--  deriving Show

--instance Show (Double -> Double) where
instance Show Solvable where
  show Speed{}      = "Speed"
  show Identity     = "Identity"
  show Translate{}  = "Translate"
  show Rotate{}     = "Rotate"
  show Select       = "Select"
  show Parent       = "Parent"
  show Controller{} = "Controller"

data RotationOrder =
  XYZ

instance Show RotationOrder where
  show XYZ = "XYZ"
