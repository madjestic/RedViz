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

module Graphics.RedViz.Entity where

import Data.UUID
import Linear.V3
import Linear.V4 ( V4(V4) )
import Linear.Quaternion
import Linear.Matrix
import Lens.Micro
import Lens.Micro.Extras

import Graphics.RedViz.Component
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Backend (BackendOptions, defaultBackendOptions)
import Graphics.RedViz.Material as R
import Graphics.RedViz.Texture hiding (uuid)
import Graphics.Rendering.OpenGL.GL.Texturing

type Object = Entity
type Camera = Entity

data Entity
  =  Entity
     { lable      :: String
     , uuid       :: UUID
     , cmps       :: [Component]
     } deriving Show

defaultEntity :: Entity -- TODO: move local properties to Components
defaultEntity =
  Entity
  { lable      = "defaultEntity"
  , uuid       = nil
  , cmps       = [] 
  } 

initObj :: Entity
initObj =
  defaultEntity
  { lable      = "initObj"
  , uuid       = nil
  , cmps       =
    [ Transformable
      { xform  =  
        (V4
         (V4 1 0 0 0)   -- <- . . . x ...
         (V4 0 1 0 0)   -- <- . . . y ...
         (V4 0 0 1 0)   -- <- . . . z-component of transform
         (V4 0 0 0 1))
      , tslvrs = []
      }
    ]
  }

-- A convenience wrapper to generate an Object Entity.  So far there are only two flavors of Entity:
-- Object and Camera, both are just Entity type-synonyms.  Distinction is arbitrary.
fromSchema :: [(Texture, TextureObject)] -> [[(Descriptor, R.Material)]]-> Schema -> IO Entity
fromSchema txTuples' dms' sch = do
  let
    obj =
      defaultEntity
      { lable = slable sch
      , uuid  = suuid  sch
      , cmps  = updateComponent <$> scmps  sch      
      }
      where
        updateComponent :: Component -> Component
        updateComponent t0@(Transformable {}) =
          t0 { xform  = foldr1 (!*!) $ xformSolver (xform t0) <$> tslvrs t0
             , tslvrs = updateSolver <$> tslvrs t0 }
          where
            updateSolver :: Component -> Component
            updateSolver slv =
              case slv of
                Identity             -> slv
                Movable _ pos _ ss   ->
                  slv { txyz   = pos }
                Turnable _ _ _ rxyz _ ss ->
                  slv { rxyz   = rxyz }
                _ -> slv                                                        
          
            xformSolver :: M44 Double -> Component -> M44 Double
            xformSolver mtx0 slv =
              case slv of
                Identity -> identity
                Movable cs pos _ _ ->
                  case cs of
                    WorldSpace  -> identity & translation .~ pos
                    ObjectSpace -> undefined
                Turnable _ _ rord rxyz _ _ -> transform' identity
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
               (identity :: M44 Double) -- TODO: add result based on solvers composition
               (backend r0)
               txTuples
               <$> concat dms :: [Drawable] }
          where          
            dms      = (dms'!!) <$> modelIDXs r0
            txs      = concatMap (\(_,m) -> R.textures m) $ concat dms :: [Texture]
            txTuples = filter (\(tx,_) -> tx `elem` txs) txTuples'     :: [(Texture, TextureObject)]

        updateComponent cmp = cmp

  return obj

-- Schema is a convenience step that fascilitates describing and generating hierarchic Entities.
-- A nested schema gets flattened to a list of entities with Parentable component,
-- propagating necessary properties from parents to children.
data Schema
  =  Schema
     { slable    :: String
     , suuid     :: UUID
     , scmps     :: [Component]
     , schildren :: [Schema]
     , sparent   :: UUID
     } deriving Show

defaultCam :: Entity
defaultCam =
  defaultEntity
  { lable      = "PlayerCamera"
  , uuid       = nil
  }

camerable :: Entity -> Component
camerable s = case camerables s of [] -> defaultCamerable; _ -> head $ camerables s

camerables :: Entity -> [Component]
camerables t = filter (\case (Camerable{}) -> True; _ -> False; ) (cmps t)


selectable :: Entity -> Component
selectable s = case selectables s of [] -> Selectable False; _ -> head $ selectables s

selectables :: Entity -> [Component]
selectables t = filter (\case (Selectable{}) -> True; _ -> False; ) (cmps t)


parentable :: Entity -> Component
parentable s = case parentables s of [] -> defaultParentable; _ -> head $ parentables s

parentables :: Entity -> [Component]
--parentables t = filter (\case (Parentable{}) -> True; _ -> False; ) (cmps t)
parentables t = [ x | x@(Parentable {} ) <- cmps t ]                  


renderable :: Entity -> Component
renderable s = case renderables s of [] -> defaultRenderable; _ -> head $ renderables s

renderables :: Entity -> [Component]
renderables t = filter (\case (Renderable{}) -> True; _ -> False; ) (cmps t)


transformable :: Entity -> Component
transformable s = case transformables s of [] -> defaultTransformable; _ -> head $ transformables s

transformables :: Entity -> [Component]
transformables t = filter (\case (Transformable{}) -> True; _ -> False; ) (cmps t)

controllable :: Entity -> Component
controllable s = case controllables s of [] -> defaultControllable; _ -> head $ controllables s

controllables :: Entity -> [Component]
controllables t = filter (\case (Controllable{}) -> True; _ -> False; ) (cmps t)
