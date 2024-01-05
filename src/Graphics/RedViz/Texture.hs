--------------------------------------------------------------------------------
-- |
-- Module      :  Texture
-- Copyright   :  (c) Vladimir Lopatin 2024
-- License     :  BSD3
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for texture handling.
--
--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Texture 
  ( Texture (..)
  , allocateTextures
  , bindTexture
  , defaultTexture
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Data.UUID
import Graphics.Rendering.OpenGL.GL (($=), blend, blendFunc, BlendingFactor(..), Capability(..), activeTexture, TextureUnit(..), GLuint)
import Graphics.Rendering.OpenGL.GL.Texturing

import Graphics.RedViz.Utils (encodeStringUUID)
import Graphics.RedViz.GLUtil.Textures (texture2DWrap)
import Graphics.RedViz.GLUtil.JuicyTextures

data Texture
  =  Texture
     { -- | Binding name in a shader.
       name :: String
       -- | A filepath to an image file location on disk, relative to project root.
     , path :: FilePath
       -- | A unique object (texture) ID.
     , uuid :: UUID
     } deriving Show
deriveJSON defaultOptions ''Texture

instance Eq Texture where
  t0 == t1 = uuid t0 == uuid t1

instance Ord Texture where
  compare t0 t1  = compare (uuid t0) (uuid t1)

-- | A default Texture type constructor.
defaultTexture :: Texture
defaultTexture
  = Texture
    "checkerboard"
    "./textures/checkerboard.png"
    (encodeStringUUID "./textures/checkerboard.png")

loadTexture :: FilePath -> IO TextureObject
loadTexture f =
  do
    t <- either error id <$> readTexture f
    texture2DWrap            $= (Repeated, ClampToEdge)
    textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
    blend                    $= Enabled
    blendFunc                $= (SrcAlpha, OneMinusSrcAlpha)
    generateMipmap' Texture2D
    return t

bindTexture :: [(UUID, GLuint)] -> Texture -> IO (Texture, TextureObject)
bindTexture hmap tx =
  do
    putStrLn $ "Binding Texture : " ++ show tx ++ " at TextureUnit : " ++ show txid
    texture Texture2D        $= Enabled
    print $ "tx : " ++ show tx
    print $ "txid : " ++ show txid
    activeTexture            $= TextureUnit txid
    tx0 <- loadTexture $ path tx --TODO : replace that with a hashmap lookup?
    textureBinding Texture2D $= Just tx0
    return (tx, tx0)
      where
        txid = fromMaybe 0 (lookup (uuid tx) hmap)
    

allocateTextures :: (Int, (Texture, TextureObject)) -> IO ()
allocateTextures (txid, (_, txo)) =
  do
    activeTexture $= TextureUnit (fromIntegral txid)
    textureBinding Texture2D $= Just txo
    return ()
