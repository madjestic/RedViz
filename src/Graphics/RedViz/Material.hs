--------------------------------------------------------------------------------
-- |
-- Module      :  Material
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD3
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling Materials.
--
--------------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Material
  ( Material (..)
  , name
  , defaultMat
  , Graphics.RedViz.Material.read
  , write
  , textures
  ) where  

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import Data.Maybe                (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Text    hiding (drop)

import Graphics.RedViz.Texture as T hiding (name, _name)

data Material
  =  Material
     { -- | Material name.
       _name       :: String
       -- | Path to vertex shader program.
     , _vertShader :: FilePath
       -- | Path to fragment shader program.
     , _fragShader :: FilePath
       -- | Paths to texture bindings and other 'Texture' data.
     , _textures   :: [Texture]  
     } deriving Show
$(makeLenses ''Material)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Material

defaultMat :: Material
defaultMat
  = Material
    "default"
    "shader.vert"
    "shader.frag"
    [defaultTexture]

-- | Read a Material json-formatted file from disk.
read :: FilePath -> IO Material
read jsonFile =
  do
    -- print $ "jsonFile :" ++ jsonFile
    d <- (eitherDecode <$> B.readFile jsonFile) :: IO (Either String Material)
    print $ "Loading Material :"
      ++ case d of
           Right m -> view name m
           _ -> "error"

    let name'       = (_name       . fromEitherDecode) d
        vertShader' = (_vertShader . fromEitherDecode) d
        fragShader' = (_fragShader . fromEitherDecode) d
        textures'   = (_textures   . fromEitherDecode) d
    return $ Material name' vertShader' fragShader' textures'
      where
        fromEitherDecode = fromMaybe (Material "" "" "" []) . fromEither
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
comp = keyOrder . fmap pack $ ["name", "fragShader", "vertShader", "textures"]
