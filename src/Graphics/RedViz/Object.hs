module Graphics.RedViz.Object where

import Data.UUID
import Linear.V3
import Linear.Matrix

import Graphics.RedViz.Transformable
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Solvable (Solvable(..), CoordSys(..), RotationOrder(..))
import Graphics.RedViz.Backend (BackendOptions, defaultBackendOptions)
import Graphics.RedViz.Material as R
import Graphics.RedViz.Texture
import Graphics.Rendering.OpenGL.GL.Texturing

data PType = Default
           | Font
           | Icon

instance Show PType where
  show Default = "Default"
  show Font    = "Font"
  show Icon    = "Icon"

data Object
  =  Object
     { transform :: Transformable
     , drws      :: [Drawable]
     , selected  :: Bool
     , uuid      :: UUID
     , active    :: Bool
     , name      :: String
     , backend   :: BackendOptions
     } deriving Show

initObj :: Object
initObj =
  Object
  { transform = defaultTransformable
  , drws     = []
  , selected = False
  , uuid     = nil
  , active   = False
  , name     = "initObj"
  }

toObject :: [(Texture, TextureObject)] -> [[(Descriptor, R.Material)]]-> PreObject -> IO Object
toObject txTuples' dms' pobj = do
  --print $ (options pobj)
  let
    dms      = (dms'!!) <$> modelIDXs pobj
    txs      = concatMap (\(_,m) -> R.textures m) $ concat dms :: [Texture]
    txTuples = filter (\(tx,_) -> tx `elem` txs) txTuples'     :: [(Texture, TextureObject)]
    drs =
      toDrawable
      (identity :: M44 Double) -- TODO: add result based on solvers composition
      (options pobj)
      txTuples
      <$> concat dms
      :: [Drawable]
    
    obj =
      Object
      { transform = defaultTransformable {tslvrs = tsolvers pobj}
      , drws      = drs
      , selected  = False
      , uuid      = puuid   pobj
      , active    = pactive pobj
      , name      = pname   pobj
      , backend   = options pobj
      }

  return obj

data PreObject
  =  PreObject
     { pname      :: String
     , ptype      :: PType
     , puuid      :: UUID
     , modelIDXs  :: [Int]
     , tsolvers   :: [Solvable] -- transformable solvers
     , posolvers  :: [Solvable] -- properties solvers
     , options    :: BackendOptions
     , pparent    :: UUID
     , pchildren  :: [PreObject]
     , pactive    :: Bool
     } deriving Show
