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
     , oslvrs    :: [Solvable]
     , uuid      :: UUID
     , parent    :: UUID
     } deriving Show

initObj :: Object
initObj =
  Object
  { transform = defaultTransformable
  , drws     = []
  , selected = False
  , oslvrs   = []
  , uuid     = nil
  , parent   = nil
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
      , oslvrs    = osolvers pobj
      , selected  = False
      , uuid      = puuid   pobj
      , parent    = pparent pobj
      }

  return obj

data PreObject
  =  PreObject
     { pname      :: String
     , ptype      :: PType
     , pidx       :: Integer
     , puuid      :: UUID
     , modelIDXs  :: [Int]
     , tsolvers   :: [Solvable] -- transformable solvers
     , osolvers   :: [Solvable] -- properties solvers
     , options    :: BackendOptions
     , pparent    :: UUID
     , pchildren  :: [PreObject]
     } deriving Show

testPreObject :: PreObject
testPreObject = 
    PreObject
    {
      pname          = "pig_object"
    , ptype          = Default
    , pidx           = 0
    , puuid          = nil
    , modelIDXs      = [0]
    , tsolvers       =
      [ Identity
      -- , Rotate
      --   { space = ObjectSpace
      --   , cxyz  = V3 0 0 0
      --   , rord  = XYZ
      --   , rxyz  = V3 0 0 (0.5)
      --   , avel  = V3 0 0 0.05 }
      , Translate
        { space   = WorldSpace
        , txyz    = V3 1.5 0 0
        , tvel    = V3 0.0 0 0
        , kinslv = Identity }
        -- , Rotate
        -- { space   = ObjectSpace
        -- , cxyz    = V3 0 0 0
        -- , rord    = XYZ
        -- , rxyz    = V3 0 0 (0.5)
        -- , avel    = V3 0 0 (0.1)
        -- , kinslv  = Identity
        --   -- Speed
        --   -- { life = 1.0
        --   -- , age  = 0.0
        --   -- , inc  = 0.01
        --   -- , amp  = 1.0
        --   -- , func = id }
        -- }
      -- , Translate
      --  { space = WorldSpace
      --  , txyz  = V3 1.1 0 0
      --  , tvel  = V3 0.0 0 0 }
      ]
    , osolvers    =
      [ Identity
      , Select
      ]
      , options   = defaultBackendOptions
      , pparent   = nil
      , pchildren =
        [ PreObject
          {
            pname          = "grid_object"
          , ptype          = Default
          , pidx           = 0
          , puuid          = nil
          , modelIDXs      = [1]
          , tsolvers       =
            [ Identity
            -- , Rotate
            --   { space = ObjectSpace
            --   , cxyz  = V3 0 0 0
            --   , rord  = XYZ
            --   , rxyz  = V3 0 0 (0.5)
            --   , avel  = V3 0 0 0.05 }
            , Translate
              { space   = WorldSpace
              , txyz    = V3 1.5 0 0
              , tvel    = V3 0.0 0 0
              , kinslv = Identity }
              , Rotate
              { space   = ObjectSpace
              , cxyz    = V3 0 0 0
              , rord    = XYZ
              , rxyz    = V3 0 0 (0.5)
              , avel    = V3 0 0 (0.02)
              , kinslv  = Identity
                -- Speed
                -- { life = 1.0
                -- , age  = 0.0
                -- , inc  = 0.01
                -- , amp  = 1.0
                -- , func = id }
              }
            -- , Translate
            --  { space = WorldSpace
            --  , txyz  = V3 1.1 0 0
            --  , tvel  = V3 0.0 0 0 }
              , Parent 
            ]
          , osolvers  =
            [ Identity
            , Select
            ]
            , options   = defaultBackendOptions
            , pparent   = nil
            , pchildren = []
          }            
        ]
    }      
