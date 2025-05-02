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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Graphics.RedViz.Texture 
  ( Texture (..)
  , allocateTextures
  , bindTexture
  , defaultTexture
  , loadTexture
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Data.UUID
import Data.Proxy
import Graphics.Rendering.OpenGL.GL (($=), blend, blendFunc, BlendingFactor(..), Capability(..), activeTexture, TextureUnit(..), GLuint, DataType (..))
import Graphics.Rendering.OpenGL.GL.Texturing
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import GHC.Generics
import Data.Binary as B
import Data.Hashable
import Data.StateVar as SV

import Graphics.RedViz.Utils (encodeStringUUID, word32ToInt, intToWord32)
import Graphics.RedViz.GLUtil.Textures
import Graphics.RedViz.GLUtil.JuicyTextures
import Control.Concurrent (threadDelay)

instance Binary Texture where
  put (Texture n p u) = do
    put n
    put p 
    put u 
  get = do
    n <- B.get 
    p <- B.get 
    u <- B.get 
    return $ Texture n p u

instance Eq Texture where
  t0 == t1 = uuid t0 == uuid t1

data Texture
  =  Texture
     { -- | Binding name in a shader.
       name :: String
       -- | A filepath to an image file location on disk, relative to project root.
     , path :: FilePath
       -- | A unique object (texture) ID.
     , uuid :: UUID
     } deriving (Show, Generic, Hashable)
deriveJSON defaultOptions ''Texture

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
    activeTexture $= TextureUnit txid
    tx0 <- case path tx of
      "shadowMap" -> freshTexture 512 512 TexMono (Proxy :: Proxy Word8)
      _ -> loadTexture $ path tx --TODO : replace that with a hashmap lookup?
    textureBinding Texture2D $= Just tx0
    return (tx, tx0)
      where
        txid = fromMaybe 0 (lookup (uuid tx) hmap)
    

allocateTextures :: Program -> (Int, (Texture, TextureObject)) -> IO ()
allocateTextures program' (txid, (tex, txo)) =
  do
    activeTexture $= TextureUnit (intToWord32 txid)
    textureBinding Texture2D $= Just txo
    location <- SV.get (uniformLocation program' $ name tex)
    uniform location $= TextureUnit (intToWord32 txid)
