{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Graphics.RedViz.GLTF.Model
  ( Mesh
  , MeshPrimitive

  , Stuff(..)
  , mergeStuff
  , unzipStuff

  , StuffLike
  , mergeStuffLike

  , VertexAttrs(..)
  ) where

import RIO

import Codec.GlTF.Material qualified as GlTF (Material)
import Data.Semigroup (Semigroup(..))
import Geomancy (Vec2)
import Geomancy.Vec3 qualified as Vec3
import Geomancy.Vec4 qualified as Vec4
import Graphics.Gl.Block (Block)
import Graphics.Gl.Block qualified as Block
import RIO.List qualified as List
import RIO.Vector qualified as Vector

type Mesh = Vector MeshPrimitive

type MeshPrimitive = (Maybe (Int, GlTF.Material), Stuff)

type IndicesType = Word32        

data Stuff = Stuff
  { sPositions :: Vector Vec3.Packed
  , sIndices   :: Vector IndicesType
  , sAttrs     :: Vector VertexAttrs
  }
  deriving (Eq, Show, Generic)

instance Semigroup Stuff where
  {-# INLINE (<>) #-}
  a <> b = mergeStuff [a, b]

  {-# INLINE sconcat #-}
  sconcat = mergeStuff  

instance Monoid Stuff where
  mempty = Stuff
    { sPositions = mempty
    , sIndices   = mempty
    , sAttrs     = mempty
    }

  {-# INLINE mconcat #-}
  mconcat = mergeStuff

mergeStuff :: Foldable t => t Stuff -> Stuff
mergeStuff source = Stuff
  { sPositions = Vector.concat allPositions
  , sIndices   = Vector.concat offsetIndices
  , sAttrs     = Vector.concat allAttrs
  }
  where
    (allPositions, allAttrs, numPositions, allIndices) = unzipStuff source

    offsetIndices = List.zipWith applyOffset chunkOffsets allIndices
      where
        applyOffset off = fmap (+ off)

        chunkOffsets = List.scanl' (+) 0 numPositions

unzipStuff
  :: Foldable t
  => t Stuff
  -> ( [Vector Vec3.Packed]
     , [Vector VertexAttrs]
     , [Word32]
     , [Vector IndicesType]
     )
unzipStuff source = List.unzip4 do
  Stuff{..} <- toList source
  pure
    ( sPositions
    , sAttrs
    , fromIntegral $ Vector.length sPositions {- sic! -}
    , sIndices
    )

type StuffLike attrs = (Vector Vec3.Packed, Vector Word32, Vector attrs)

mergeStuffLike :: Foldable t => t (StuffLike attrs) -> (StuffLike attrs)
mergeStuffLike source =
  ( Vector.concat allPositions
  , Vector.concat offsetIndices
  , Vector.concat allAttrs
  )
  where
    (allPositions, allIndices, allAttrs) = List.unzip3 (toList source)

    offsetIndices = List.zipWith applyOffset chunkOffsets allIndices
      where
        applyOffset off = fmap (+ fromIntegral off)

    chunkOffsets = List.scanl' (+) 0 $ map Vector.length allPositions

data VertexAttrs = VertexAttrs
  { vaTexCoord :: Vec2
  , vaRGBA     :: Vec4.Vec4
  , vaNormal   :: Vec3.Packed
  }
  deriving (Eq, Ord, Show, Generic, Block)
  deriving Storable via (Block.Packed VertexAttrs)
