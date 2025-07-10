--------------------------------------------------------------------------------
-- |
-- Module      :  Texture
-- Copyright   :  (c) Vladimir Lopatin 2025
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
  , bindTextureUniform
  , bindTexture
  , defaultTexture
  , loadTexture
  , writeTexture
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Data.UUID
import Data.Proxy
import Graphics.Rendering.OpenGL.GL
       ( ($=), blend, blendFunc, BlendingFactor(..)
       , Capability(..), activeTexture, TextureUnit(..)
       , GLuint, DataType (..), PixelData(..), PixelFormat(..))
import Graphics.Rendering.OpenGL.GL.Texturing
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import GHC.Generics
import Data.Binary as B
import Data.Hashable
import Data.StateVar as SV

import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Marshal.Alloc (free)

import Data.Vector (Vector (..), fromList, (!))

import Graphics.RedViz.Utils (encodeStringUUID, word32ToInt, intToWord32, intToWord8)
import Graphics.RedViz.GLUtil.Textures
import Graphics.RedViz.GLUtil.JuicyTextures

import Codec.Picture.Types
import Codec.Picture (saveBmpImage, savePngImage)
import Control.Concurrent (threadDelay)
import Control.Exception
import Graphics.GL.Functions (glReadPixels)
import Graphics.GL.Tokens

instance Binary Texture where
  put (Texture n p w h u) = do
    put n
    put p 
    put w
    put h
    put u 
  get = do
    n <- B.get 
    p <- B.get 
    w <- B.get 
    h <- B.get 
    u <- B.get 
    return $ Texture n p w h u

instance Eq Texture where
  t0 == t1 = uuid t0 == uuid t1

data Texture
  =  Texture
     { -- | Binding name in a shader.
       name :: String
       -- | A filepath to an image file location on disk, relative to project root.
     , path :: FilePath
       -- | A unique object (texture) ID.
     , width  :: Int
     , height :: Int
     , uuid   :: UUID
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
    1024
    1024    
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
    
bindTextureUniform :: Program -> (Int, (Texture, TextureObject)) -> IO ()
bindTextureUniform program' (txid, (tex, txo)) =
  do
    activeTexture $= TextureUnit (intToWord32 txid)
    textureBinding Texture2D $= Just txo
    location <- SV.get (uniformLocation program' $ name tex)
    uniform location $= TextureUnit (intToWord32 txid)

-- linearize :: Float -> Float -> Float -> Float
-- linearize z near far = depth'
--   where
--     ndc    = z * 2.0 - 1.0
--     depth' = (2.0 * near * far) / (far + near - ndc * (far - near))

writeTexture :: (Int, (Texture, TextureObject)) -> FilePath -> IO ()
writeTexture (txid, (tex, txo)) filepath =
  do
    activeTexture $= TextureUnit (intToWord32 txid)
    textureBinding Texture2D $= Just txo
    let pixelCount = width tex * height tex
        byteCount = pixelCount
        -- nearPlane  = 1
        -- farPlane   = 100

    pixelData <- mallocArray byteCount :: IO (Ptr Float)
    getTexImage Texture2D 0 (PixelData DepthComponent Float pixelData)
    pixelsList <- peekArray byteCount pixelData
    let pixels = fromList pixelsList :: Vector Float
    --print pixels
    free pixelData
    let
      image = generateImage
          (\y x ->
            let 
              idx = y * width tex + x
              r = (pixels ! idx) --linearize nearPlane farPlane (pixels ! idx)
            in (r :: PixelF))
          (width tex) (height tex)
      dynamicImage = ImageYF image

    result <- try (savePngImage filepath dynamicImage) :: IO (Either IOException ())
    case result of
      Right () -> putStrLn $ "Depth map saved to " ++ filepath
      Left err -> putStrLn $ "Failed to save depth map: " ++ show err
