{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.RedViz.Backend where

import Data.Aeson hiding (Options)
import GHC.Generics
import Data.Binary  
import Data.Hashable

import Graphics.Rendering.OpenGL hiding (blendFunc, get)

data Backend
  = OpenGL
  | Vulkan

instance Binary (Color4 GLfloat) where
  put (Color4 r g b a) = do put r
                            put g
                            put b
                            put a  

  get = do r <- get
           g <- get 
           b <- get 
           a <- get 
           return (Color4 r g b a)

instance Binary PrimitiveMode where
   put Points        = do put (0  :: Word8) 
   put Lines         = do put (1  :: Word8) 
   put LineLoop      = do put (2  :: Word8) 
   put LineStrip     = do put (3  :: Word8) 
   put Triangles     = do put (4  :: Word8) 
   put TriangleStrip = do put (5  :: Word8) 
   put TriangleFan   = do put (6  :: Word8) 
   put Quads         = do put (7  :: Word8) 
   put QuadStrip     = do put (8  :: Word8) 
   put Polygon       = do put (9  :: Word8) 
   put Patches       = do put (10 :: Word8)
   get = do tag <- getWord8
            case tag of
              0  -> return Points       
              1  -> return Lines         
              2  -> return LineLoop      
              3  -> return LineStrip     
              4  -> return Triangles     
              5  -> return TriangleStrip 
              6  -> return TriangleFan   
              7  -> return Quads         
              8  -> return QuadStrip     
              9  -> return Polygon       
              10 -> return Patches       

instance Binary Capability where
  put Disabled = do put (0 :: Word8)
  put Enabled  = do put (1 :: Word8)
  get = do tag <- getWord8
           case tag of
             0 -> return Disabled
             1 -> return Enabled

instance Binary BlendingFactor where
  put Zero                  = do put (0  :: Word8) 
  put One                   = do put (1  :: Word8) 
  put SrcColor              = do put (2  :: Word8) 
  put OneMinusSrcColor      = do put (3  :: Word8) 
  put DstColor              = do put (4  :: Word8) 
  put OneMinusDstColor      = do put (5  :: Word8) 
  put SrcAlpha              = do put (6  :: Word8) 
  put OneMinusSrcAlpha      = do put (7  :: Word8) 
  put DstAlpha              = do put (8  :: Word8) 
  put OneMinusDstAlpha      = do put (9  :: Word8) 
  put ConstantColor         = do put (10 :: Word8) 
  put OneMinusConstantColor = do put (11 :: Word8) 
  put ConstantAlpha         = do put (12 :: Word8) 
  put OneMinusConstantAlpha = do put (13 :: Word8) 
  put SrcAlphaSaturate      = do put (14 :: Word8) 
  get = do tag <- getWord8
           case tag of
             0  -> return Zero
	     1	-> return One			  
	     2	-> return SrcColor		  
	     3	-> return OneMinusSrcColor	  
	     4	-> return DstColor		  
	     5	-> return OneMinusDstColor	  
	     6	-> return SrcAlpha		  
	     7	-> return OneMinusSrcAlpha	  
	     8	-> return DstAlpha		  
	     9	-> return OneMinusDstAlpha	  
	     10 -> return ConstantColor	  
	     11 -> return OneMinusConstantColor 
	     12 -> return ConstantAlpha	  
	     13 -> return OneMinusConstantAlpha 
	     14 -> return SrcAlphaSaturate	  

instance Hashable (Color4 GLfloat) where
  hashWithSalt salt (Color4 r g b a) = 
    foldl (\acc x -> acc + hashFloat x) salt [r, g, b, a]
    where
      hashFloat :: GLfloat -> Int
      hashFloat = fromIntegral . floor . (* 1000) . realToFrac

instance Hashable PrimitiveMode where
  hashWithSalt salt Points        = hashWithSalt salt (0 :: Int)  -- Assuming 0 is the first element in enum
  hashWithSalt salt Lines         = hashWithSalt salt (1 :: Int)  -- Second element, so value 1
  hashWithSalt salt LineLoop      = hashWithSalt salt (2 :: Int)
  hashWithSalt salt LineStrip     = hashWithSalt salt (3 :: Int)
  hashWithSalt salt Triangles     = hashWithSalt salt (4 :: Int)
  hashWithSalt salt TriangleStrip = hashWithSalt salt (5 :: Int)
  hashWithSalt salt TriangleFan   = hashWithSalt salt (6 :: Int)
  hashWithSalt salt Quads         = hashWithSalt salt (7 :: Int)
  hashWithSalt salt QuadStrip     = hashWithSalt salt (8 :: Int)
  hashWithSalt salt Polygon       = hashWithSalt salt (9 :: Int)
  hashWithSalt salt Patches       = hashWithSalt salt (10 :: Int)  -- Assuming 10 is the last element in enum

instance Hashable Capability where
    hashWithSalt salt Disabled = hashWithSalt salt (0 :: Int)
    hashWithSalt salt Enabled  = hashWithSalt salt (1 :: Int)

instance Hashable BlendingFactor where
    hashWithSalt salt Zero                  = hashWithSalt salt (0 :: Int) 
    hashWithSalt salt One                   = hashWithSalt salt (1 :: Int) 
    hashWithSalt salt SrcColor              = hashWithSalt salt (2 :: Int) 
    hashWithSalt salt OneMinusSrcColor      = hashWithSalt salt (3 :: Int) 
    hashWithSalt salt DstColor              = hashWithSalt salt (4 :: Int) 
    hashWithSalt salt OneMinusDstColor      = hashWithSalt salt (5 :: Int) 
    hashWithSalt salt SrcAlpha              = hashWithSalt salt (6 :: Int) 
    hashWithSalt salt OneMinusSrcAlpha      = hashWithSalt salt (7 :: Int) 
    hashWithSalt salt DstAlpha              = hashWithSalt salt (8 :: Int) 
    hashWithSalt salt OneMinusDstAlpha      = hashWithSalt salt (9 :: Int) 
    hashWithSalt salt ConstantColor         = hashWithSalt salt (10:: Int) 
    hashWithSalt salt OneMinusConstantColor = hashWithSalt salt (11:: Int) 
    hashWithSalt salt ConstantAlpha         = hashWithSalt salt (12:: Int) 
    hashWithSalt salt OneMinusConstantAlpha = hashWithSalt salt (13:: Int) 
    hashWithSalt salt SrcAlphaSaturate      = hashWithSalt salt (14:: Int) 

-- instance Hashable BlendingFactor where
--     hash _ = hash (0 :: Int)
    -- hash One = hash 1
    -- hash SrcColor = hash 2
    -- hash OneMinusSrcColor = hash 3
    -- hash DstColor = hash 4
    -- hash OneMinusDstColor = hash 5
    -- hash SrcAlpha = hash 6
    -- hash OneMinusSrcAlpha = hash 7
    -- hash DstAlpha = hash 8
    -- hash OneMinusDstAlpha = hash 9
    -- hash ConstantColor = hash 10
    -- hash OneMinusConstantColor = hash 11
    -- hash ConstantAlpha = hash 12
    -- hash OneMinusConstantAlpha = hash 13
    -- hash SrcAlphaSaturate = hash 14

-- instance Hashable BlendingFactor where
--   hashWithSalt = hashWithSalt

data Options
  =  Options
     {
       primitiveMode :: PrimitiveMode -- Triangles | Points
     , bgrColor      :: Color4 GLfloat
     , ptSize        :: Float
     , depthMsk      :: Capability
     , blendFunc     :: (BlendingFactor, BlendingFactor)
     } deriving (Show, Generic, Binary, Eq, Hashable)
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
