--------------------------------------------------------------------------------
-- |
-- Module      :  Material
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling Materials.
--
--------------------------------------------------------------------------------

{-# LANGUAGE CPP    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Material
  ( Material (..)
  , defaultMat
  , Graphics.RedViz.Material.read
  , write
  ) where  

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import Data.Maybe                (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Text    hiding (drop)

import Graphics.RedViz.Texture as T hiding (name)

debug :: Bool
#ifdef DEBUG
debug = True
#else
debug = False
#endif

data Material
  =  Material
     { -- | Material name.
       name       :: String
       -- | Path to vertex shader program.
     , vertShader :: FilePath
       -- | Path to fragment shader program.
     , fragShader :: FilePath
       -- | Paths to texture bindings and other 'Texture' data.
     , geomShader :: Maybe FilePath
     , textures   :: [Texture]  
     } deriving Show
deriveJSON defaultOptions ''Material

defaultMat :: Material
defaultMat
  = Material
    "default"
    "shader.vert"
    "shader.frag"
    Nothing
    [defaultTexture]

-- | Read a Material json-formatted file from disk.
read :: FilePath -> IO Material
read jsonFile =
  do
    -- print $ "jsonFile :" ++ jsonFile
    d <- (eitherDecode <$> B.readFile jsonFile) :: IO (Either String Material)

    when debug $
      print $ "Loading Material :"
        ++ case d of
             Right m -> name m
             _ -> "error"

    let name'       = (name       . fromEitherDecode) d
        vertShader' = (vertShader . fromEitherDecode) d
        fragShader' = (fragShader . fromEitherDecode) d
        geomShader' = (geomShader . fromEitherDecode) d
        textures'   = (textures   . fromEitherDecode) d
    return $ Material name' vertShader' fragShader' geomShader' textures'
      where
        fromEitherDecode = fromMaybe (Material "" "" "" Nothing []) . fromEither
        fromEither d =
          case d of
            Right pt -> Just pt
            _ -> Nothing

-- | Write a Material json-formatted file to disk.
write :: Material -> FilePath -> IO ()
write mat fileOut =
  do
    B.writeFile fileOut $ encodePretty' config  mat
    where
      config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . fmap pack $ ["name", "fragShader", "vertShader", "geomShader", "textures"]
