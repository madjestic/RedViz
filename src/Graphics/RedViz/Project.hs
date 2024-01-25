module Graphics.RedViz.Project
  ( Project (..)
  , sharedFonts
  , setProjectUUID
  , flatten
  ) where

import Data.UUID
import Data.UUID.V4

import Graphics.RedViz.Entity

data Project
  =  Project
     {
       projname       :: String
     , resx           :: Int
     , resy           :: Int
     , camMode        :: String
     , models         :: [FilePath]
     , fontModels     :: [FilePath]
     , iconModels     :: [FilePath]     
     , preObjects     :: [PreObject]
     , preFontObject  :: [PreObject]
     , preIconObject  :: [PreObject]     
     , pcameras       :: [Camera]
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

setProjectUUID :: Project -> IO Project
setProjectUUID prj0 = do
  pobjs' <- mapM setUUID (preObjects prj0)
  return prj0 { preObjects = pobjs' }

setUUID :: PreObject -> IO PreObject
setUUID pobj0@(PreObject{pchildren = []}) = do
  genUUID  <- nextRandom :: IO UUID
  return pobj0 { puuid = genUUID }
setUUID pobj0@(PreObject{pchildren = [p]}) = do
  genUUID  <- nextRandom :: IO UUID
  genUUID' <- nextRandom :: IO UUID
  return pobj0 { puuid     = genUUID
               , pchildren = [p { pparent = genUUID
                                , puuid   = genUUID'}] }
setUUID pobj0@(PreObject{pchildren = (p:ps)}) = do
  genUUID  <- nextRandom :: IO UUID
  p'  <- setUUID p --(p{pparent = genUUID})
  ps' <- mapM setUUID ps
  return pobj0 { puuid     = genUUID 
               , pchildren = p':ps' }

-- this is flattening the (pre)object tree into a list of (pre)objects
-- TODO: pass on tsolvers from parent to children
flatten :: PreObject -> [PreObject]
flatten pobj0@(PreObject{pchildren = []})     = [childFree pobj0]
flatten pobj0@(PreObject{pchildren = [p]})    = childFree pobj0 : flatten p
flatten pobj0@(PreObject{pchildren = (p:ps)}) = childFree pobj0 : concat (flatten p : (flatten <$> ps))

childFree :: PreObject -> PreObject
childFree pobj0 = (pobj0{pchildren = []})
