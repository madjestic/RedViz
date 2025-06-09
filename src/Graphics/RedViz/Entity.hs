--------------------------------------------------------------------------------
-- |
-- Module      :  Entity
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic camera structure.
--
--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Graphics.RedViz.Entity where

import Data.UUID
import Linear.V3
import Linear.Quaternion
import Linear.Matrix
import Lens.Micro
import Data.Binary
import GHC.Generics
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Hashable

import Graphics.RedViz.Component as C
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Material as R
import Graphics.RedViz.Texture hiding (uuid)
import Graphics.Rendering.OpenGL.GL.Texturing
--import Debug.Trace as DT

type Object = Entity
type Camera = Entity
type Light  = Entity

data Entity
  =  Entity
     { lable :: String
     , uuid  :: UUID
     , cmps  :: [Component]
     } deriving (Show, Generic, Binary)

-- TODO: finish merge/match Entities
mergeEntity :: Entity -> Entity -> Entity
mergeEntity e0 eS@entitySave = 
  case (uuid e0) == (uuid eS) of
    True  -> e0 { lable = lable eS
                , cmps  = zipWith mergeComponents (cmps e0) (cmps eS)}
    False -> error "mismatching Entity IDs found"

defaultEntity :: Entity -- TODO: move local properties to Components
defaultEntity =
  Entity
  { lable      = "defaultEntity"
  , uuid       = nil
  , cmps       = [] 
  }

-- A convenience wrapper to generate an Object Entity.  So far there are only two flavors of Entity:
-- Object and Camera, both are just Entity type-synonyms.  Distinction is arbitrary.
fromSchema :: [(Texture, TextureObject)] -> [[(Descriptor, R.Material)]]-> Schema -> IO Entity
fromSchema txTuples' dms' sch = do
  (shadowProgram, dtx) <- genObscurable
  return $ genEntity (shadowProgram, dtx)
  where
    genEntity (shadowProgram, dtx) =
      defaultEntity
      { lable = slable sch
      , uuid  = suuid  sch
      , cmps  = updateComponent <$> scmps  sch      
      }
      where
        updateComponent :: Component -> Component
        updateComponent t0@(Transformable {}) = --DT.trace ("xform : " ++ show (xform t0)) $ t0
          t0 { xform  = foldl (!*!) (xform t0) $ xformSolver (xform t0) <$> tslvrs t0
             , tslvrs = updateSolver <$> tslvrs t0 }
          where
            updateSolver :: Component -> Component
            updateSolver c0 =
              case c0 of
                Identity            -> c0
                Movable _ vel0 _   ->
                  c0 { tvel   = vel0 }
                Turnable _ _ _ rxyz _ _ ->
                  c0 { rxyz   = rxyz }
                --Parentable {} -> Parentable { parent = suuid sch }
                _ -> c0                                                        
          
            xformSolver :: M44 Double -> Component -> M44 Double
            xformSolver mtx0 c0 =
              case c0 of
                Identity   -> identity
                PreTransformable txyz rord rxyz -> rotate' identity
                  where
                    rotate' :: M44 Double -> M44 Double
                    rotate' mtx0' = mtx
                      where
                        mtx =
                          mkTransformationMat
                          rot
                          tr
                          where
                            rot    = 
                              identity !*!
                              case rord of
                                XYZ ->
                                      fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (rxyz^._x)) -- pitch
                                  !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (rxyz^._y)) -- yaw
                                  !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (rxyz^._z)) -- roll
                            tr     = (identity::M44 Double)^.translation + txyz

                Movable cs vel0 _ ->
                  case cs of
                    WorldSpace  -> identity & translation .~ vel0
                    ObjectSpace -> undefined
                Turnable _ rord _ rxyz _ _ -> transform' identity
                  where
                    transform' :: M44 Double -> M44 Double
                    transform' mtx0' = mtx
                      where
                        mtx =
                          mkTransformationMat
                          rot
                          tr
                          where
                            rot    = 
                              identity !*!
                              case rord of
                                XYZ ->
                                      fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (rxyz^._x)) -- pitch
                                  !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (rxyz^._y)) -- yaw
                                  !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (rxyz^._z)) -- roll
                            tr     = (identity::M44 Double)^.translation
                _ -> identity
        updateComponent r0@(Renderable {}) =
          r0 { drws =
                 toDrawable
                 (identity :: M44 Double)
                 (backend r0)
                 txTuples
                 <$> concat dms :: [Drawable] }
          where          
            dms      = (dms'!!) <$> modelIDXs r0 :: [[(Descriptor, Material)]]
            txs      = concatMap (\(_,m) -> R.textures m) $ concat dms :: [Texture]
            txTuples = filter (\(tx,_) -> tx `elem` txs) txTuples'     :: [(Texture, TextureObject)]
        updateComponent r0@(Obscurable {}) = 
          r0 { program = Just shadowProgram
             , dtx     = Just dtx }
        updateComponent cmp = cmp

-- Schema is a convenience step that fascilitates describing and generating hierarchic Entities.
-- A nested schema gets flattened to a list of entities with Parentable component,
-- propagating necessary properties from parents to children.

-- instance Hashable Game where
--   hashWithSalt salt (Game t m q me p c u o w) = hashWithSalt salt t

-- instance Hashable Schema where
--      hashWithSalt salt schema = hashWithSalt salt (slable schema, suuid schema, scmps schema, schildren schema, sparent schema)

-- hashToUUID :: Schema -> UUID
-- hashToUUID =
--   case (fromByteString . BS.pack . hash) of
--     Just s -> s
--     Nothing -> nil

data Schema
  =  Schema
     { slable    :: String
     , suuid     :: UUID
     , scmps     :: [Component]
     , schildren :: [Schema]
     , sparent   :: UUID
     } deriving (Eq, Show, Generic, Binary, Hashable)

camerable :: Entity -> Component
camerable s = fromMaybe defaultCamerable (listToMaybe . camerables $ s)

camerables :: Entity -> [Component]
camerables t = [ x | x@(Camerable {} ) <- cmps t ]

lightable :: Entity -> Component
lightable s = fromMaybe defaultLightable (listToMaybe . lightables $ s)

lightables :: Entity -> [Component]
lightables t = [ x | x@(Lightable {} ) <- cmps t ]

selectable :: Entity -> Component
selectable s = fromMaybe (Selectable False) (listToMaybe . selectables $ s)

selectables :: Entity -> [Component]
selectables t = [ x | x@(Selectable {} ) <- cmps t ]

renderable :: Entity -> Component
renderable s = fromMaybe defaultRenderable (listToMaybe . renderables $ s)

renderables :: Entity -> [Component]
renderables t = [ x | x@(Renderable {} ) <- cmps t ]

obscurable :: Entity -> Component
obscurable s = fromMaybe defaultObscurable (listToMaybe . obscurables $ s)

obscurables :: Entity -> [Component]
obscurables t = [ x | x@(Obscurable {} ) <- cmps t ]

transformable :: Entity -> Component
transformable s = fromMaybe defaultTransformable (listToMaybe . transformables $ s)

transformables :: Entity -> [Component]
transformables t = [ x | x@(Transformable {} ) <- cmps t ]

turnable :: Entity -> Component
turnable s = fromMaybe defaultTurnable (listToMaybe . turnables $ s)

turnables :: Entity -> [Component]
turnables t = [ x | x@(Turnable {} ) <- cmps t ]

parentable :: Entity -> Component
parentable s = fromMaybe (Parentable nil) (listToMaybe . parentables $ s)

parentables :: Entity -> [Component]
parentables t = [ x | x@(Parentable {} ) <- tslvrs . transformable $ t ]

parents :: Object -> [Object] -> [Object]
parents obj0 = filter (\o -> uuid o == (parent . parentable $ obj0))

controllable :: Entity -> Component
controllable s = fromMaybe (error "Not a Controllable") (listToMaybe . controllables $ s)

controllables :: Entity -> [Component]
controllables t = [ x | x@(Controllable {} ) <- tslvrs . transformable $ t ]

movable :: Entity -> Component
movable s = case listToMaybe . movables $ s of Nothing -> error "Not a Movable" ; Just a -> a

movables :: Entity -> [Component]
movables t = case transformables t of
  [] -> []
  _ -> [ x | x@(Movable {} ) <- tslvrs . transformable $ t ]

attractable :: Entity -> Component
attractable s = fromMaybe defaultAttractable (listToMaybe . attractables $ s)

attractables :: Entity -> [Component]
attractables t = case movables t of
  [] -> []
  _  -> [ x | x@(Attractable {} ) <- kslvrs . movable $ t ]

measurable :: Entity -> Component
measurable s = fromMaybe (error "Not a Measurable") (listToMaybe . measurables $ s)

measurables :: Entity -> [Component]
measurables t = [ x | x@(Measurable {} ) <- cmps t ]
