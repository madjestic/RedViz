module Graphics.RedViz.Project
  ( Project (..)
  , sharedFonts
  , setProjectUUID
  , setProjectUUID'
  , setUUID'
  , nextRandomD
  , flatten
  , wordToFloat
  , floatToWord32
  , wordToDouble
  , doubleToWord32
  , intToWord8
  ) where

import Data.UUID
import Data.UUID.V4
import Data.Binary (decode,encode,Binary)
  
import Data.Maybe
import Data.ByteString.Lazy as BSL hiding (concat)
import Data.ByteString as BS hiding (concat)
import Data.Word (Word32, Word8)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Base (castSTUArray)
import GHC.ST (runST, ST)
import Data.Hashable

import Graphics.RedViz.Entity
import Graphics.RedViz.Utils

-------------------------------------------------------------------------------

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord32 :: Float -> Word32
floatToWord32 x = runST (cast x)

wordToDouble :: Word32 -> Double
wordToDouble x = runST (cast x)

doubleToWord32 :: Double -> Word32
doubleToWord32 x = runST (cast x)

intToWord8 :: Int -> Word8
intToWord8 x = 0 -- runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

-------------------------------------------------------------------------------  

data Project
  =  Project
     {
       projname   :: String
     , resx       :: Int
     , resy       :: Int
     , camMode    :: String
     , models     :: [FilePath]
     , fontModels :: [FilePath]
     , iconModels :: [FilePath]     
     , pobjects   :: [Schema]
     , pfonts     :: [Schema]
     , picons     :: [Schema]     
     , pcameras   :: [Schema]
     } deriving Show

sharedFonts :: [FilePath]
sharedFonts =
  [ "models/fnt_space.gltf"
  , "models/fnt_0.gltf"
  , "models/fnt_1.gltf"
  , "models/fnt_2.gltf"
  , "models/fnt_3.gltf"
  , "models/fnt_4.gltf"
  , "models/fnt_5.gltf"
  , "models/fnt_6.gltf"
  , "models/fnt_7.gltf"
  , "models/fnt_8.gltf"
  , "models/fnt_9.gltf"
  , "models/fnt_a.gltf"
  , "models/fnt_b.gltf"
  , "models/fnt_c.gltf"
  , "models/fnt_d.gltf"
  , "models/fnt_e.gltf"
  , "models/fnt_f.gltf"
  , "models/fnt_g.gltf"
  , "models/fnt_h.gltf"
  , "models/fnt_i.gltf"
  , "models/fnt_j.gltf"
  , "models/fnt_k.gltf"
  , "models/fnt_l.gltf"
  , "models/fnt_m.gltf"
  , "models/fnt_n.gltf"
  , "models/fnt_o.gltf"
  , "models/fnt_p.gltf"
  , "models/fnt_q.gltf"
  , "models/fnt_r.gltf"
  , "models/fnt_s.gltf"
  , "models/fnt_t.gltf"
  , "models/fnt_u.gltf"
  , "models/fnt_v.gltf"
  , "models/fnt_w.gltf"
  , "models/fnt_x.gltf"
  , "models/fnt_y.gltf"
  , "models/fnt_z.gltf"
  , "models/fnt_plus.gltf"
  , "models/fnt_minus.gltf"
  , "models/fnt_equal.gltf"
  , "models/fnt_gt.gltf"
  , "models/fnt_comma.gltf"
  , "models/fnt_dot.gltf"
  , "models/fnt_question.gltf"
  , "models/fnt_exclam.gltf"
  , "models/fnt_asterics.gltf"
  , "models/fnt_slash.gltf"
  , "models/fnt_semicolon.gltf"
  , "models/fnt_quote.gltf"
  , "models/fnt_A.gltf"
  , "models/fnt_B.gltf"
  , "models/fnt_C.gltf"
  , "models/fnt_D.gltf"
  , "models/fnt_E.gltf"
  , "models/fnt_F.gltf"
  , "models/fnt_G.gltf"
  , "models/fnt_H.gltf"
  , "models/fnt_I.gltf"
  , "models/fnt_J.gltf"
  , "models/fnt_K.gltf"
  , "models/fnt_L.gltf"
  , "models/fnt_M.gltf"
  , "models/fnt_N.gltf"
  , "models/fnt_O.gltf"
  , "models/fnt_P.gltf"
  , "models/fnt_Q.gltf"
  , "models/fnt_R.gltf"
  , "models/fnt_S.gltf"
  , "models/fnt_T.gltf"
  , "models/fnt_U.gltf"
  , "models/fnt_V.gltf"
  , "models/fnt_W.gltf"
  , "models/fnt_X.gltf"
  , "models/fnt_Y.gltf"
  , "models/fnt_Z.gltf"
  , "models/fnt_crosshair.gltf"
  ]    

-- Deterministic UUIDs
setProjectUUID' :: Project -> Project
setProjectUUID' prj0 = 
  prj0 { pobjects = pobjs'
       , pcameras = pcams' }
  where
    pobjs' = setUUID' <$> pobjects prj0
    pcams' = setUUID' <$> pcameras prj0

setUUID' :: Schema -> Schema
setUUID' pobj0@(Schema{schildren = []}) =
  pobj0 { suuid = nextRandomD pobj0}
setUUID' pobj0@(Schema{schildren = [p]}) =
  pobj0 { suuid     = nextRandomD pobj0
        , schildren = [p { sparent = nextRandomD pobj0
                         , suuid   = nextRandomD p }] }
setUUID' pobj0@(Schema{schildren = (p:ps)}) =
  pobj0 { suuid     = nextRandomD pobj0
        , schildren = p':ps' }
  where
    p'  = setUUID' p
    ps' = setUUID' <$> ps

nextRandomD :: Schema -> UUID
nextRandomD = 
  hashUUID

-- Non-deterministic UUIDs
setProjectUUID :: Project -> IO Project
setProjectUUID prj0 = do
  pobjs' <- mapM setUUID (pobjects prj0)
  pcams' <- mapM setUUID (pcameras prj0)
  return prj0 { pobjects = pobjs'
              , pcameras = pcams' }

setUUID :: Schema -> IO Schema
setUUID pobj0@(Schema{schildren = []}) = do
  genUUID  <- nextRandom :: IO UUID
  return pobj0 { suuid = genUUID }
setUUID pobj0@(Schema{schildren = [p]}) = do
  genUUID  <- nextRandom :: IO UUID
  genUUID' <- nextRandom :: IO UUID
  return pobj0 { suuid     = genUUID
               , schildren = [p { sparent = genUUID
                                , suuid   = genUUID'}] }
setUUID pobj0@(Schema{schildren = (p:ps)}) = do
  genUUID  <- nextRandom :: IO UUID
  p'  <- setUUID p --(p{sparent = genUUID})
  ps' <- mapM setUUID ps
  return pobj0 { suuid     = genUUID 
               , schildren = p':ps' }

-- this is flattening the (pre)object tree into a list of (pre)objects
-- TODO: pass on tsolvers from parent to children
flatten :: Schema -> [Schema]
flatten pobj0@(Schema{schildren = []})     = [childFree pobj0]
flatten pobj0@(Schema{schildren = [p]})    = childFree pobj0 : flatten p
flatten pobj0@(Schema{schildren = (p:ps)}) = childFree pobj0 : concat (flatten p : (flatten <$> ps))

childFree :: Schema -> Schema
childFree pobj0 = (pobj0{schildren = []})
