{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.RedViz.Backend where

import Data.Aeson hiding (Options)
import GHC.Generics

import Graphics.Rendering.OpenGL hiding (blendFunc)

data Backend
  = OpenGL
  | Vulkan

data Options -- TODO: rename to Options?
  =  Options
     {
       primitiveMode :: PrimitiveMode -- Triangles | Points
     , bgrColor      :: Color4 GLfloat
     , ptSize        :: Float
     , depthMsk      :: Capability
     , blendFunc     :: (BlendingFactor, BlendingFactor)
     } deriving (Generic, Show)
instance FromJSON Options

instance ToJSON Capability where
  toJSON c = case c of
    Enabled  -> "Enabled"
    Disabled -> "Disabled"

instance ToJSON BlendingFactor where
  toJSON c =
    case c of
      Zero		    -> "Zero"
      One		    -> "One"
      SrcColor		    -> "SrcColor"
      OneMinusSrcColor	    -> "OneMinusSrcColor"
      DstColor		    -> "DstColor"
      OneMinusDstColor	    -> "OneMinusDstColor"
      SrcAlpha		    -> "SrcAlpha"
      OneMinusSrcAlpha	    -> "OneMinusSrcAlpha"
      DstAlpha		    -> "DstAlpha"
      OneMinusDstAlpha	    -> "OneMinusDstAlpha"
      ConstantColor	    -> "ConstantColor"
      OneMinusConstantColor -> "OneMinusConstantColor"	
      ConstantAlpha	    -> "ConstantAlpha"
      OneMinusConstantAlpha -> "OneMinusConstantAlpha"	
      SrcAlphaSaturate	    -> "SrcAlphaSaturate"
      _ -> "Blending Factor Undefined"

instance FromJSON BlendingFactor where
  parseJSON (String o) =
    case o of
      "Zero"                  -> return Zero                 
      "One"                   -> return One                  
      "SrcColor"              -> return SrcColor             
      "OneMinusSrcColor"      -> return OneMinusSrcColor     
      "DstColor"              -> return DstColor             
      "OneMinusDstColor"      -> return OneMinusDstColor     
      "SrcAlpha"              -> return SrcAlpha             
      "OneMinusSrcAlpha"      -> return OneMinusSrcAlpha     
      "DstAlpha"              -> return DstAlpha             
      "OneMinusDstAlpha"      -> return OneMinusDstAlpha     
      "ConstantColor"         -> return ConstantColor        
      "OneMinusConstantColor" -> return OneMinusConstantColor
      "ConstantAlpha"         -> return ConstantAlpha        
      "OneMinusConstantAlpha" -> return OneMinusConstantAlpha
      "SrcAlphaSaturate"      -> return SrcAlphaSaturate     
      _ -> error $ "Invalid BlendingFactor: " ++ show o
  parseJSON _ = error "Json format not exptected"

instance FromJSON Capability where
  parseJSON (String o) =
    case o of
      "Enabled"  -> return Enabled
      "Disabled" -> return Disabled
      _ -> error $ "Invalid Capability: " ++ show o
  parseJSON _ = error "Json format not exptected"

instance ToJSON Options

instance ToJSON PrimitiveMode where
  toJSON p = case p of
    Points        -> "Points"
    Lines         -> "Lines"
    LineLoop      -> "LineLoop"
    LineStrip     -> "LineStrip"
    Triangles     -> "Triangles"
    TriangleStrip -> "TriangleStrip"
    TriangleFan   -> "TriangleFan"
    Quads         -> "Quads" 
    QuadStrip     -> "QuadStrip"
    Polygon       -> "Polygon"
    Patches       -> "Patches"

instance FromJSON PrimitiveMode where
  parseJSON (String o) =
    case o of
      "Points"        -> return Points
      "Lines"         -> return Lines
      "LineLoop"      -> return LineLoop
      "LineStrip"     -> return LineStrip
      "Triangles"     -> return Triangles
      "TriangleStrip" -> return TriangleStrip
      "TriangleFan"   -> return TriangleFan
      "Quads"         -> return Quads
      "QuadStrip"     -> return QuadStrip
      "Polygon"       -> return Polygon
      "Patches"       -> return Patches
      _           -> error $ "Invalid PrimitiveMode: " ++ show o
  parseJSON _ = error "Json format not exptected"

instance ToJSON (Color4 GLfloat) where
  toJSON (Color4 r g b a) =
    object ["red" .= r, "green" .= g, "blue" .= b, "alpha" .= a]

instance FromJSON (Color4 GLfloat) where
  parseJSON (Object o) =
    do
      red   <- o .: "red"
      green <- o .: "green"
      blue  <- o .: "blue"
      alpha <- o .: "alpha"
      return $ Color4 red green blue alpha
  parseJSON _ = error "Json format not exptected"

defaultOptions :: Options
defaultOptions = defOpts
  
defOpts :: Options
defOpts =
      Options
      { primitiveMode = Triangles
      , bgrColor      = Color4 0.5 0.0 0.0 1.0
      , ptSize        = 1.0
      , depthMsk      = Enabled
      --, blendFunc     = (SrcColor, Zero)
      , blendFunc     = (SrcColor, OneMinusSrcAlpha)
      }

linesOpts :: Options
linesOpts =
      Options
      { primitiveMode = Lines
      , bgrColor      = Color4 0.0 0.5 0.0 1.0
      , ptSize        = 1.0
      , depthMsk      = Enabled
      , blendFunc     = (SrcColor, Zero)
      }

pointsOpts :: Options
pointsOpts =
      Options
      { primitiveMode = Points
      , bgrColor      = Color4 0.0 0.5 0.0 1.0
      , ptSize        = 3.0
      , depthMsk      = Enabled
      , blendFunc     = (SrcColor, Zero)
      }     
