{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Graphics.RedViz.Component where

import Linear.V3
import Linear.V4
import Linear.Matrix
import Data.UUID 
import Data.Binary  
import GHC.Generics
import Data.Hashable
import Graphics.Rendering.OpenGL (TextureObject (..), Program (..))

import Graphics.RedViz.Drawable
import Graphics.RedViz.Backend (Options, defaultOptions)
import Graphics.RedViz.Texture (Texture)

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving (Show, Generic, Binary, Eq, Hashable)

instance Binary (Double -> Double) where
  put _ = do put ()
  get   = do return id

instance Eq (Double -> Double) where
    f == g = and [abs (f x - g x) < tolerance | x <- inputs]
        where
            tolerance :: Double
            tolerance = 1e-9  
            inputs :: [Double]
            inputs = takeWhile (<= 10) $ iterate (*2) 1  -- Example range of inputs

instance Binary Component where
  put Identity = do
    put (0 :: Word8)
    put ()
  put Constant = do
    put (1 :: Word8)
    put ()
  put (Movable s t k)  = do
    put (2 :: Word8)
    put s
    put t
    put k
  put (Turnable s r c rx a k) = do
    put (3 :: Word8)
    put s
    put r
    put c
    put rx
    put a
    put k
  put (Selectable b) = do
    put (4 :: Word8)
    put b
  put (Controllable cv cy cyp m r mo p ph) = do
    put (5 :: Word8)
    put cv
    put cy
    put cyp
    put m
    put r
    put mo
    put p
    put ph
  put (Parentable p) = do
    put (6 :: Word8)
    put p
  put (Fadable l a i am f) = do
    put (7 :: Word8)
    put l  
    put a 
    put i 
    put am 
    put f
  put (Attractable m a f ft) = do
    put (8 :: Word8)
    put m
    put a 
    put f 
    put ft
  put (Measurable m) = do
    put (9 :: Word8)
    put m
  put (Transformable x t) = do
    put (10 :: Word8)
    put x
    put t
  put (PreTransformable t r rx) = do
    put (11 :: Word8)
    put t
    put r
    put rx
  put (Camerable a f) = do
    put (12 :: Word8)
    put a
    put f
  put (Renderable m d a b) = do
    put (13 :: Word8)
    put m
    put d
    put a
    put b

  get = do
    t <- get :: Get Word8
    case t of
      0 -> return Identity
      1 -> return Constant
      2 -> do
        s <- get 
        t <- get
        k <- get
        return $ Movable s t k
      3 -> do
        s  <- get 
        r  <- get 
        c  <- get 
        rx <- get 
        a  <- get 
        k  <- get
        return $ Turnable s r c rx a k
      4 -> do
        b <- get
        return $ Selectable b
      5 -> do
        cv  <- get 
        cy  <- get 
        cyp <- get 
        m   <- get 
        r   <- get 
        mo  <- get 
        p   <- get 
        ph  <- get
        return $ Controllable cv cy cyp m r mo p ph
      6 -> do
        p <- get
        return $ Parentable p
      7 -> do
        l  <- get 
        a  <- get 
        i  <- get 
        am <- get 
        f  <- get
        return $ Fadable l a i am f
      8 -> do
        m  <- get 
        a  <- get 
        f  <- get 
        ft <- get
        return $ Attractable m a f ft
      9 -> do
        m <- get
        return $ Measurable m
      10 -> do
        x <- get
        t <- get
        return $ Transformable x t
      11 -> do
        t  <- get 
        r  <- get 
        rx <- get 
        return $ PreTransformable t r rx
      12 -> do
        a <- get 
        f <- get
        return $ Camerable a f
      13 -> do
        m <- get 
        d <- get
        a <- get 
        b <- get
        return $ Renderable m d a b

instance Hashable (Double -> Double) where
  hashWithSalt = hashWithSalt

mergeComponents :: Component -> Component -> Component
mergeComponents c0 cS =
  case (c0, cS) of
    (Renderable {}, Renderable {}) -> c0
    _ -> cS
    
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
  | Obscurable
    { program :: Maybe Program
    , dtx     :: Maybe (Int, (Texture, TextureObject))
    }
  deriving (Eq, Generic, Hashable)


data Physics =
    Static
  | Dynamic
    deriving (Show, Generic, Binary, Eq, Hashable)

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
  show (Obscurable p d) =
    "Obscurable :" ++ show p ++ "\n" ++ show d ++ "\n"
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
  deriving (Show, Generic, Binary, Eq, Hashable)

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

defaultObscurable :: Component
defaultObscurable = Obscurable
  { program = Nothing
  , dtx     = Nothing
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
  , mouseS = -0.00005   -- mouse    "sensitivity"
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

