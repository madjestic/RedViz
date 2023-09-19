--------------------------------------------------------------------------------
-- |
-- Module      :  Texture
-- Copyright   :  (c) Vladimir Lopatin 2022
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
  -- , name
  -- , path
  -- , uuid
  , defaultTexture
  ) where

--import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import Data.UUID
import Data.Text    hiding (drop)

import Graphics.RedViz.Utils (encodeStringUUID)

data Texture
  =  Texture
     { -- | Binding name in a shader.
       name :: String
       -- | A filepath to an image file location on disk, relative to project root.
     , path :: FilePath -- TODO: replace with Maybe FilePath or Either (FilePath or Generated, maybe a formula?)
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

comp :: Text -> Text -> Ordering
comp = keyOrder . fmap pack $ ["name", "path", "uuid"]
