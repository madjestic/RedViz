--------------------------------------------------------------------------------
-- |
-- Module      :  Drawable
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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Graphics.RedViz.Drawable where

import Graphics.Rendering.OpenGL (TextureObject (..), genObjectName)
import Linear.Matrix (M44, M33, _m33, mkTransformationMat, identity, translation, (*!!), (*!))
import Linear.Vector ((*^))  
import Linear.V3
import Lens.Micro
import GHC.Generics
import Data.Binary
import Data.Hashable
-- import Data.Maybe
-- import Data.List (sortBy)
-- import Data.Ord (comparing, Down (..))

--import Graphics.RedViz.LoadShaders --(createShaderProgram, loadShaders)
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Backend (Options)
import Graphics.RedViz.Material as R
import Graphics.RedViz.Texture
import Graphics.RedViz.Utils (encodeStringUUID)
--import Graphics.RedViz.LoadShaders (ShaderInfo (..))
--import Debug.Trace as DT

instance Hashable TextureObject where
  hashWithSalt salt (TextureObject tid) = hashWithSalt salt tid

data Drawable
  =  Drawable
     { descriptor :: Descriptor
     , material   :: Material
     , dtxs       :: [(Int, (Texture, TextureObject))]
     , doptions   :: Options
     , u_xform    :: M44 Double
     } deriving (Show, Eq, Generic, Hashable)

instance Binary TextureObject where
  put (TextureObject v) = do
    put v 
  get = do
    v <- get
    return $ TextureObject v

instance Binary Drawable where
  put (Drawable d m dt dop u) = do
    put d
    put m
    put dt
    put dop
    put u
  get = do
    d   <- get
    m   <- get
    dt  <- get
    dop <- get
    u   <- get
    return $ Drawable d m dt dop u

toDrawable
  :: M44 Double
  -> Options
  -> [(Texture, TextureObject)]
  -> (Descriptor, R.Material)
  -> Drawable
toDrawable xform' opts txos (d, mat') = dr
  where
    txs'   = R.textures mat'
    txos'  = zip [0..] $ unzipWith txs' txos :: [(Int, (Texture, TextureObject))] 
      where
        unzipWith :: Eq a => [a] -> [(a,b)] -> [(a,b)]
        unzipWith xs xys = xys'
          where
            xys' = filter (\xy -> fst xy `elem` xs) xys
    dr =
      Drawable
      { 
        u_xform    = xform'
      , descriptor = d
      , material   = mat'
      , dtxs       = txos'
      , doptions   = opts
      }

-- Shader sources
depthVertexShaderSrc :: String
depthVertexShaderSrc = unlines [
    "#version 330 core",
    "uniform mat4 lightViewProjection;",
    "in vec3 position;",
    "void main() {",
    "    gl_Position = lightViewProjection * vec4(position, 1.0);",
    "}"
    ]

genDepthTexture :: IO (Int, (Texture, TextureObject))
genDepthTexture = do
  let depthTexture = Texture "shadowMap" "shadowMap" 1024 1024 (encodeStringUUID "shadowMap")
  depthTextureObject <- genObjectName
  return (0, (depthTexture, depthTextureObject))

-- depthMapShaderInfo :: ShaderInfo
-- depthMapShaderInfo = ShaderInfo
--   VertexShader (FileSource "mat/depthmap/src/shader.vert")

-- genObscurable :: IO (Program, (Int, (Texture, TextureObject))) -- it's 
-- genObscurable = do 
--   --depthProgram <- createShaderProgram depthVertexShaderSrc Nothing
--   depthProgram <- loadShaders [depthMapShaderInfo]
--   dtx          <- genDepthTexture
--   return (depthProgram, dtx)

data Alignment =
   TL |TC |TR
  |CL |CC |CR
  |BL |BC |BR
  deriving (Show, Generic, Binary)

data Format -- move to Format.hs?
  =  Format
     { alignment :: Alignment
     , xres      :: Int
     , yres      :: Int
     , xoffset   :: Double
     , yoffset   :: Double
     , zoffset   :: Double
     , soffset   :: Double -- scale Offset
     , ssize     :: Double -- scale Size
     }
  deriving (Show, Generic, Binary)

type CursorPos = (Integer, Integer)

formatText :: Format -> [Drawable] -> [String] -> CursorPos -> [Drawable]
formatText _ _ [] _  = []
formatText fmt drws [s] (x,y) =
  formatString fmt drws s (x,y)
formatText fmt drws (s:ss) (x,y) =
  formatText fmt drws [s] (x,y) ++ formatText fmt drws ss (x,y+1)

formatString :: Format -> [Drawable] -> String -> CursorPos -> [Drawable]
formatString _ _ [] _ = []
formatString fmt drws [c]    (x,y) = [formatChar fmt drws c (x,y)]
formatString fmt drws (c:cs) (x,y) =  formatChar fmt drws c (x,y) : formatString fmt drws cs (x+1,y)

formatChar :: Format -> [Drawable] -> Char -> CursorPos -> Drawable
formatChar _ drws chr cpos =
  case chr of
    ' ' -> offsetDrw cpos (drws!!0)
    '0' -> offsetDrw cpos (drws!!1)
    '1' -> offsetDrw cpos (drws!!2)
    '2' -> offsetDrw cpos (drws!!3)
    '3' -> offsetDrw cpos (drws!!4)
    '4' -> offsetDrw cpos (drws!!5)
    '5' -> offsetDrw cpos (drws!!6)
    '6' -> offsetDrw cpos (drws!!7)
    '7' -> offsetDrw cpos (drws!!8)
    '8' -> offsetDrw cpos (drws!!9)
    '9' -> offsetDrw cpos (drws!!10)
    'a' -> offsetDrw cpos (drws!!11)
    'b' -> offsetDrw cpos (drws!!12)
    'c' -> offsetDrw cpos (drws!!13)
    'd' -> offsetDrw cpos (drws!!14)
    'e' -> offsetDrw cpos (drws!!15)
    'f' -> offsetDrw cpos (drws!!16)
    'g' -> offsetDrw cpos (drws!!17)
    'h' -> offsetDrw cpos (drws!!18)
    'i' -> offsetDrw cpos (drws!!19)
    'j' -> offsetDrw cpos (drws!!20)
    'k' -> offsetDrw cpos (drws!!21)
    'l' -> offsetDrw cpos (drws!!22)
    'm' -> offsetDrw cpos (drws!!23)
    'n' -> offsetDrw cpos (drws!!24)
    'o' -> offsetDrw cpos (drws!!25)
    'p' -> offsetDrw cpos (drws!!26)
    'q' -> offsetDrw cpos (drws!!27)
    'r' -> offsetDrw cpos (drws!!28)
    's' -> offsetDrw cpos (drws!!29)
    't' -> offsetDrw cpos (drws!!30)
    'u' -> offsetDrw cpos (drws!!31)
    'v' -> offsetDrw cpos (drws!!32)
    'w' -> offsetDrw cpos (drws!!33)
    'x' -> offsetDrw cpos (drws!!34)
    'y' -> offsetDrw cpos (drws!!35)
    'z' -> offsetDrw cpos (drws!!36)
    '+' -> offsetDrw cpos (drws!!37)
    '-' -> offsetDrw cpos (drws!!38)
    '=' -> offsetDrw cpos (drws!!39)
    '>' -> offsetDrw cpos (drws!!40)
    ',' -> offsetDrw cpos (drws!!41)
    '.' -> offsetDrw cpos (drws!!42)
    '?' -> offsetDrw cpos (drws!!43)
    '!' -> offsetDrw cpos (drws!!44)
    '*' -> offsetDrw cpos (drws!!45)
    '/' -> offsetDrw cpos (drws!!46)
    ';' -> offsetDrw cpos (drws!!47)
    '\''-> offsetDrw cpos (drws!!48)
    'A' -> offsetDrw cpos (drws!!49)
    'B' -> offsetDrw cpos (drws!!50)
    'C' -> offsetDrw cpos (drws!!51)
    'D' -> offsetDrw cpos (drws!!52)
    'E' -> offsetDrw cpos (drws!!53)
    'F' -> offsetDrw cpos (drws!!54)
    'G' -> offsetDrw cpos (drws!!55)
    'H' -> offsetDrw cpos (drws!!56)
    'I' -> offsetDrw cpos (drws!!57)
    'J' -> offsetDrw cpos (drws!!58)
    'K' -> offsetDrw cpos (drws!!59)
    'L' -> offsetDrw cpos (drws!!60)
    'M' -> offsetDrw cpos (drws!!61)
    'N' -> offsetDrw cpos (drws!!62)
    'O' -> offsetDrw cpos (drws!!63)
    'P' -> offsetDrw cpos (drws!!64)
    'Q' -> offsetDrw cpos (drws!!65)
    'R' -> offsetDrw cpos (drws!!66)
    'S' -> offsetDrw cpos (drws!!67)
    'T' -> offsetDrw cpos (drws!!68)
    'U' -> offsetDrw cpos (drws!!69)
    'V' -> offsetDrw cpos (drws!!70)
    'W' -> offsetDrw cpos (drws!!71)
    'X' -> offsetDrw cpos (drws!!72)
    'Y' -> offsetDrw cpos (drws!!73)
    'Z' -> offsetDrw cpos (drws!!74)
    _   -> head drws

  
offsetDrw :: CursorPos -> Drawable -> Drawable
offsetDrw cpos drw =
  drw { u_xform = mkTransformationMat rot tr }
  where
    sh  = 0.1
    sv  = -0.15
    rot = identity :: M33 Double
    tr  =
      (identity::M44 Double)^.translation
      +
      V3 (fromIntegral $ fst cpos) (fromIntegral $ snd cpos) 0.0
      *
      V3 sh sv 0.0

scaleDrws :: Format -> [Drawable] -> [Drawable]
scaleDrws fmt drws =
  scaleDrw fmt <$> drws

scaleDrw :: Format -> Drawable -> Drawable
scaleDrw fmt drw =
  drw { u_xform = mkTransformationMat rot tr }
  where
    ss  = ssize   fmt
    so  = soffset fmt
    ss' = ss * 0.0025
    rot = ss' *!! u_xform drw^._m33 :: M33 Double
    tr  = so * ss' *^ (u_xform drw  :: M44 Double)^.translation
