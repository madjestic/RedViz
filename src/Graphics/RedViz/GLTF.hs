--------------------------------------------------------------------------------
-- |
-- Module      :  GLTF
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Drawable data type and related structures.
--
--------------------------------------------------------------------------------
{-# LANGUAGE ImportQualifiedPost #-}

module Graphics.RedViz.GLTF where

import Graphics.RedViz.GLTF.Load (loadMeshPrimitives)
import Graphics.RedViz.GLTF.Model as Model
import Graphics.RedViz.Material as R

import Codec.GlTF.Material as GltfM
import Text.GLTF.Loader    as Gltf hiding (Texture, Material (..))

import Data.Coerce (coerce)
import Data.Maybe
import Data.Vector qualified as V hiding (head, length)
import Data.Word
import Data.Text (pack, unpack)
import Geomancy.Vec2 hiding (dot, normalize)
import Geomancy.Vec3 hiding (dot, normalize)
import Geomancy.Vec4 hiding (dot, normalize)
import Lens.Micro
import Linear.V3
import Graphics.Rendering.OpenGL (GLenum, GLfloat)

defaultGltfMat :: GltfM.Material
defaultGltfMat =  GltfM.Material
  { emissiveFactor       = (0,0,0)
  , alphaMode            = MaterialAlphaMode {unMaterialAlphaMode = pack $ "OPAQUE"}
  , alphaCutoff          = 0.5
  , doubleSided          = False
  , pbrMetallicRoughness = Nothing
  , normalTexture        = Nothing
  , occlusionTexture     = Nothing
  , emissiveTexture      = Nothing
  , GltfM.name           = Just . pack $ "test"
  , extensions           = Nothing
  , extras               = Nothing
  }

fromGltfMat :: GltfM.Material -> IO R.Material
fromGltfMat mat =
  R.read 
  $ case GltfM.name mat of
      Nothing -> "./mat/checkerboard/checkerboard"
      Just s  -> "./mat/" ++ unpack s ++ "/" ++ unpack s

loadGltf :: FilePath -> IO ([[([GLenum], [Float])]], [GltfM.Material])
loadGltf fp = do
  --(root, meshPrimitives) <- loadMeshPrimitives False False fp
  (_, meshPrimitives) <- loadMeshPrimitives False False fp
  let
    mgrs = V.toList <$> V.toList meshPrimitives :: [[Model.MeshPrimitive]]
    positions = (fmap.fmap) (\(_, stuff) -> sPositions stuff) mgrs :: [[V.Vector Packed]]
    indices   = (fmap.fmap) (\(_, stuff) -> sIndices   stuff) mgrs 
    idx       = (fmap.fmap) V.toList indices 
    attrs     = (fmap.fmap) (\(_, stuff) -> sAttrs     stuff) mgrs :: [[V.Vector VertexAttrs]]
    uvs       = (fmap.fmap) vaTexCoord <$> (fmap.fmap) V.toList attrs 
    colors    = (fmap.fmap) vaRGBA     <$> (fmap.fmap) V.toList attrs
    --normals   = (fmap.fmap.fmap) vaNormal   $ (fmap.fmap) V.toList attrs
    matTuples = (fmap.fmap) (\(maybeMatTuple, _) -> fromMaybe (0, defaultGltfMat) maybeMatTuple) mgrs :: [[(Int, GltfM.Material)]]
    mats      = (fmap.fmap) snd matTuples :: [[GltfM.Material]]

    ps = (fmap.fmap) (fromVec3' . unPacked) <$> ((fmap.fmap) V.toList positions) :: [[[(Float,Float,Float)]]]
    cs = (fmap.fmap.fmap) fromVec4' colors :: [[[(Float,Float,Float,Float)]]]
    ts = (fmap.fmap.fmap) fromVec2' uvs
    d = (,,) <$$$.> ps <***.> cs <***.> ts
  --verts = (fmap.fmap.concatMap) (\((x,y,z),(cr,cg,cb,ca),(u,v)) -> [x,y,z,cr,cg,cb,u,v]) d
    verts = (fmap.fmap.concatMap) (\((x,y,z),(cr,cg,cb,ca),(u,v)) -> [x,y,z,cr,cg,cb,ca,u,v]) d -- TODO: add Alpha!
  return $ (zipWith zip idx verts, concat mats)

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = fmap

(<$$$.>) :: (a -> b) -> [[[a]]] -> [[[b]]]
(<$$$.>) = fmap . fmap . fmap

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

(<***.>) :: [[[a -> b]]] -> [[[a]]] -> [[[b]]]
(<***.>) =  (zipWith . zipWith . zipWith) ($)

fromVec2' :: Vec2 -> (Float, Float)
fromVec2' xy = withVec2 (coerce xy) (,)
  
fromVec3' :: Vec3 -> (Float, Float, Float)
fromVec3' xyz = withVec3 (coerce xyz) (,,)

fromVec4' :: Vec4 -> (Float, Float, Float, Float)
fromVec4' xyzw = withVec4 (coerce xyzw) (,,,)

getVertices :: Gltf -> V.Vector (V3 Float)
getVertices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitivePositions) (mesh ^. _meshPrimitives)

getIndices :: Gltf -> V.Vector Word16
getIndices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitiveIndices) (mesh ^. _meshPrimitives)
