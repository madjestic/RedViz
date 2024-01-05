--------------------------------------------------------------------------------
-- |
-- Copyright 2023 IC Rainbow
-- source: https://hackage.haskell.org/package/keid-resource-gltf
--
--------------------------------------------------------------------------------
-- |
-- Module      :  GLTF Load
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic camera structure.
--
--------------------------------------------------------------------------------
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}

module Graphics.RedViz.GLTF.Load
  ( loadMeshPrimitives
  , loadGlb
  , loadGlbChunks
  , loadGltf
  , loadUri
  ) where

import RIO

import Codec.GLB qualified as GLB
import Codec.GlTF qualified as GlTF
import Codec.GlTF.Accessor qualified as Accessor hiding (AccessorSparseIndices(..), AccessorSparseValues(..), AccessorSparse(..))
import Codec.GlTF.Buffer qualified as Buffer
import Codec.GlTF.BufferView qualified as BufferView
import Codec.GlTF.Material qualified as Material
import Codec.GlTF.Mesh qualified as Mesh
import Codec.GlTF.Root qualified as Root
import Codec.GlTF.URI qualified as URI
import Data.ByteString.Unsafe qualified as ByteString
import Foreign (sizeOf, peekArray, castPtr)
import Geomancy (Vec2, Vec4)
import Geomancy.Vec3 qualified as Vec3
import RIO.ByteString qualified as ByteString
import RIO.FilePath (takeDirectory, takeExtensions, (</>))
import RIO.HashMap qualified as HashMap
import RIO.List qualified as List
import RIO.Vector qualified as Vector
import Data.Vector (iforM)
import Resource.Compressed.Zstd qualified as Zstd

import Graphics.RedViz.GLTF.Model (MeshPrimitive, Stuff(..), VertexAttrs(..))

--import Debug.Trace as DT

loadGlb :: FilePath -> IO (Either String (ByteString, Root.GlTF))
loadGlb sceneFile =
  loadGlbChunks sceneFile >>= \case
    Right chunks ->
      case Vector.toList chunks of
        [] ->
          pure . Left $ "No chunks in GLB file " <> show sceneFile
        [_root] ->
          pure . Left $ "No data chunk in GLB file " <> show sceneFile
        gltf : buffer : _rest ->
          pure $
            fmap (GLB.chunkData buffer,) $
              GlTF.fromChunk gltf
    Left err ->
      pure $ Left err

loadGlbChunks :: FilePath -> IO (Either String (Vector GLB.Chunk))
loadGlbChunks sceneFile =
  Zstd.fromFileWith (pure . GLB.fromByteString) GLB.fromFile sceneFile >>= \case
    Right glb ->
      pure $ Right (GLB.chunks glb)
    Left (_offset, err) ->
      pure $ Left err

loadGltf :: FilePath -> IO (Either String Root.GlTF)
loadGltf =
  Zstd.fromFileWith (pure . GlTF.fromByteString) GlTF.fromFile

loadUri :: FilePath -> FilePath -> IO (Either a ByteString)
loadUri sceneFile uri =
  fmap Right . Zstd.fromFileWith pure ByteString.readFile $
    takeDirectory sceneFile </> uri

-- XXX: OTOH, it may be better to unfold scene first 🤔
loadMeshPrimitives
  :: (
   --  MonadReader env m
   --, HasLogFunc env
     MonadThrow m
     , MonadUnliftIO m
   --, MonadIO m
     )
  => Bool
  -> Bool
  -> FilePath
  -- -> m (Vector (Vector ()))
  -> m
    ( Root.GlTF
    , Vector (Vector MeshPrimitive)
    )
loadMeshPrimitives reverseIndices addBacksides fp = do
  --logInfo $ "Loading scene from " <> fromString fp

  (glbData, root) <- liftIO $
    if takeExtensions fp `elem` [".glb", ".glb.zst"] then
      loadGlb fp >>= \case
        Left err ->
          throwString $ "GLB load error: " <> err
        Right (buffer, root) ->
          pure (Just buffer, root)
    else
      loadGltf fp >>= \case
        Left err ->
          throwString $ "glTF load error: " <> err
        Right root ->
          pure (Nothing, root)

  buffers <- case Root.buffers root of
    Nothing ->
      pure mempty
    Just buffers ->
      for buffers \case
        Buffer.Buffer{Buffer.uri=Nothing} ->
          case glbData of
            Nothing ->
              throwString $ "Empty buffer URI in " <> show fp -- XXX: not loading GLB, are we?
            Just bs ->
              pure bs
        Buffer.Buffer{Buffer.uri=Just path} ->
          liftIO (URI.loadURI (loadUri fp) path) >>= \case
            Left err ->
              throwString $ "Buffer load failed for " <> show path <> ": " <> err
            Right bs ->
              pure bs

  let
    getBuffer bix =
      case buffers Vector.!? Buffer.unBufferIx bix of
        Nothing ->
          throwString $ show bix <> " not present in " <> show fp-- :: IO ByteString
        Just buffer ->
          pure buffer

  getAccessor <- case Root.accessors root of
    Nothing ->
      throwString $ "No accessors in " <> fp
    Just accessors ->
      pure \aix ->
        case accessors Vector.!? Accessor.unAccessorIx aix of
          Nothing ->
            throwString $ show aix <> " not present in " <> show fp-- :: IO Accessor.Accessor
          Just accessor ->
            --DT.trace ("accessor : " ++ show accessor) 
            pure accessor

  getBufferView <- case Root.bufferViews root of
    Nothing ->
      throwString $ "No buffer views in " <> fp
    Just bufferViews ->
      pure \bvix ->
        case bufferViews Vector.!? BufferView.unBufferViewIx bvix of
          Nothing ->
            throwString $ show bvix <> " not present in " <> show fp-- :: IO BufferView.BufferView
          Just bufferView ->
            pure bufferView

  let materials = fromMaybe mempty $ Root.materials root

  --DT.traceShowM ("root : ", root)
  meshPrimitives <- case Root.meshes root of
    Nothing ->
      throwString $ "No meshes in " <> fp
    Just meshes ->
      --for (Vector.zip (Vector.fromList [0 :: Int ..]) meshes) \(_meshIx, mesh) -> do
      iforM meshes \_meshIx mesh -> do
        iforM (Mesh.primitives mesh) \_primIx prim -> do
          -- for (Vector.zip (Vector.fromList [0 :: Int ..]) (Mesh.primitives mesh)) \(_primIx, prim) -> do
          --   DT.traceShowM
          --     ( "meshIx"    , _meshIx
          --     , "Mesh.name" , Mesh.name mesh
          --     , "_primIx"   , _primIx
          --     , "material"  , Mesh.material prim
          --     )
          case Mesh.mode prim of
            Mesh.TRIANGLES ->
              pure ()
            mode ->
              throwString $ "Can't load anything but TRIANGLES, got " <> show mode

          indicesCCW <- case Mesh.indices prim of
            Nothing ->
              throwString "No indices for mesh primitive"
            Just aix -> do
              Accessor.Accessor{Accessor.componentType} <- getAccessor aix
              case componentType of
                Accessor.UNSIGNED_INT   ->
                  accessBuffer @Word32 getAccessor getBufferView getBuffer Accessor.SCALAR Accessor.UNSIGNED_INT   aix
                Accessor.UNSIGNED_SHORT -> fmap (fmap fromIntegral) $
                  accessBuffer @Word16 getAccessor getBufferView getBuffer Accessor.SCALAR Accessor.UNSIGNED_SHORT aix
                Accessor.UNSIGNED_BYTE  -> fmap (fmap fromIntegral) $
                  accessBuffer @Word8  getAccessor getBufferView getBuffer Accessor.SCALAR Accessor.UNSIGNED_BYTE  aix
                e ->
                  throwM $ UnexpectedComponentType aix Accessor.UNSIGNED_INT e
                  
          (material, indices) <- case Mesh.material prim of
            Nothing ->
              pure (Nothing, indicesCCW)
            Just (Material.MaterialIx mix) ->
              case materials Vector.!? mix of
                Nothing ->
                  throwString "No material for index"
                Just mat@Material.Material{Material.doubleSided} -> do
                  pure
                    ( Just (mix, mat)
                    , if doubleSided && addBacksides then
                        indicesCCW <> reverse indicesCCW
                      else
                        if reverseIndices then
                          reverse indicesCCW
                        else
                          indicesCCW
                    )

          positions <- case HashMap.lookup "POSITION" (Mesh.attributes prim) of
            Nothing ->
              -- XXX: huh?
              throwString $ "Mesh primitive without POSITION attribute"
            Just aix ->
              accessBuffer @Vec3.Packed getAccessor getBufferView getBuffer Accessor.VEC3 Accessor.FLOAT aix
  
          normals <- case HashMap.lookup "NORMAL" (Mesh.attributes prim) of
            Nothing -> do
              pure $ take (length positions) $ List.repeat (Vec3.Packed 0)
            Just aix ->
              accessBuffer @Vec3.Packed getAccessor getBufferView getBuffer Accessor.VEC3 Accessor.FLOAT aix

          texCoords0 <- case HashMap.lookup "TEXCOORD_0" (Mesh.attributes prim) of
            Nothing -> do
              pure $ take (length positions) $ List.repeat 0
            Just aix ->
              accessBuffer @Vec2 getAccessor getBufferView getBuffer Accessor.VEC2 Accessor.FLOAT aix

          -- tangents <- case HashMap.lookup "TANGENT" (Mesh.attributes prim) of
          --   Just aix ->
          --     accessBuffer @Vec4 getAccessor getBufferView getBuffer Accessor.VEC4 Accessor.FLOAT aix
          --   Nothing -> do
          --     pure $ take (length positions) $ List.repeat 0

          _rgba <- case HashMap.lookup "_RGBA" (Mesh.attributes prim) of
            Nothing -> do
              pure $ take (length positions) $ List.repeat 0
            Just aix ->
              accessBuffer @Vec4 getAccessor getBufferView getBuffer Accessor.VEC4 Accessor.FLOAT aix

          let
            attrs = do
              (uvw, norm, rgba) <- List.zip3 texCoords0 normals _rgba
              pure VertexAttrs
                { vaTexCoord = uvw
                , vaNormal   = norm
                , vaRGBA     = rgba
                }
          pure
            ( material
            , Stuff
                { sPositions = Vector.fromList positions
                , sAttrs     = Vector.fromList attrs
                , sIndices   = Vector.fromList indices
                }
            )

  pure (root, meshPrimitives)

accessBuffer
  :: forall a m
  .  ( MonadThrow m
     , MonadIO m
     , Storable a
     )
  => (Accessor.AccessorIx -> m Accessor.Accessor)
  -> (BufferView.BufferViewIx -> m BufferView.BufferView)
  -> (Buffer.BufferIx -> m ByteString)
  -> Accessor.AttributeType
  -> Accessor.ComponentType
  -> Accessor.AccessorIx
  -> m [a]
accessBuffer getAccessor getBufferView getBuffer expectAttribute expectComponent aix = do
  Accessor.Accessor{Accessor.bufferView, Accessor.byteOffset=accOffset, Accessor.componentType, Accessor.count, Accessor.type'} <- getAccessor aix

  bv@BufferView.BufferView{BufferView.byteOffset=bufOffset, BufferView.byteLength} <- case bufferView of
    Nothing ->
      throwString $ "No bufferView for index accessor " <> show aix
    Just bvix ->
      getBufferView bvix

  buffer <- getBuffer (BufferView.buffer bv)

  unexpected (UnexpectedAttributeType aix) expectAttribute type'
  unexpected (UnexpectedComponentType aix) expectComponent componentType
  let strideSize = sizeOf (error "strideSize.sizeOf" :: a)
  case BufferView.byteStride bv of
    Nothing ->
      pure ()
    Just stride
      | stride == strideSize ->
          pure ()
    Just stride ->
      unexpected (UnexpectedBufferViewStride aix) strideSize stride

  let bytes = ByteString.take byteLength $ ByteString.drop (accOffset + bufOffset) buffer
  liftIO . ByteString.unsafeUseAsCString bytes $
    Foreign.peekArray count . Foreign.castPtr

unexpected
  :: (Eq e, Exception exception, MonadThrow m)
  => (e -> e -> exception)
  -> e
  -> e
  -> m ()
unexpected cons expected got =
  unless (expected == got) $
    throwM $ cons expected got

data UnexpectedAttributeType = UnexpectedAttributeType
  { uatAccessor :: Accessor.AccessorIx
  , uatExpected :: Accessor.AttributeType
  , uatGot      :: Accessor.AttributeType
  }
  deriving (Eq, Ord, Show)

instance Exception UnexpectedAttributeType

data UnexpectedComponentType = UnexpectedComponentType
  { uctAccessor :: Accessor.AccessorIx
  , uctExpected :: Accessor.ComponentType
  , uctGot      :: Accessor.ComponentType
  }
  deriving (Eq, Ord, Show)

instance Exception UnexpectedComponentType

data UnexpectedBufferViewStride = UnexpectedBufferViewStride
  { ubvsAccessor :: Accessor.AccessorIx
  , ubvsExpected :: Int
  , ubvsGot      :: Int
  }
  deriving (Eq, Ord, Show)

instance Exception UnexpectedBufferViewStride
